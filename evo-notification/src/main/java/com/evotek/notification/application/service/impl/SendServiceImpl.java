package com.evotek.notification.application.service.impl;

import com.evotek.common.client.iam.IAMClient;
import com.evotek.common.client.storage.StorageClient;
import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.iam.UserDTO;
import com.evotek.common.dto.response.storage.FileDTO;
import com.evotek.common.email.MailService;
import com.evotek.common.enums.ContentType;
import com.evotek.common.enums.EventType;
import com.evotek.common.enums.TargetType;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.StrUtils;
import com.evotek.common.webapp.i18n.LocaleStringService;
import com.evotek.notification.application.mapper.EventEntityMapper;
import com.evotek.notification.application.mapper.EventFileEntityMapper;
import com.evotek.notification.application.mapper.EventTargetEntityMapper;
import com.evotek.notification.application.mapper.NotificationEntityMapper;
import com.evotek.notification.application.service.SendService;
import com.evotek.notification.application.service.TelegramBotService;
import com.evotek.notification.domain.*;
import com.evotek.notification.infrastructure.firebase.FCMService;
import com.evotek.notification.infrastructure.firebase.FirebaseData;
import com.evotek.notification.infrastructure.persistence.entity.DeviceEntity;
import com.evotek.notification.infrastructure.persistence.entity.EventEntity;
import com.evotek.notification.infrastructure.persistence.entity.UserTelegramEntity;
import com.evotek.notification.infrastructure.persistence.repository.*;
import com.evotek.notification.infrastructure.support.enums.EventStatus;
import com.evotek.notification.infrastructure.support.util.Const;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.mail.MessagingException;
import javax.sql.DataSource;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
@Slf4j
@AutoConfigureAfter({DataSource.class})
@RequiredArgsConstructor
public class SendServiceImpl implements SendService {

    private final MailService mailService;
    private final FCMService fcmService;
    private final StorageClient storageClient;
    private final DeviceRepository deviceRepository;
    private final EventRepository eventRepository;
    private final EventEntityMapper eventEntityMapper;
    private final NotificationRepository notificationRepository;
    private final NotificationEntityMapper notificationEntityMapper;
    private final EventFileRepository eventFileRepository;
    private final EventFileEntityMapper eventFileEntityMapper;
    private final EventTargetRepository eventTargetRepository;
    private final EventTargetEntityMapper eventTargetEntityMapper;
    private final IAMClient iamClient;
    private final LocaleStringService localeStringService;
    private final UserTelegramRepository userTelegramRepository;
    private final TelegramBotService telegramBotService;

    @Value("${app.storage.folder_temporary}")
    private String folderTemporary;

    @Override
    public void sendViaEmail(Event event) {
        List<String> emailUsers = event.getTargets().stream()
                .map(Target::getEmail).distinct().collect(Collectors.toList());
        log.info("Start send event {}, emails {}", event.getId(), emailUsers);

        List<String> fileNames = new ArrayList<>();
        List<String> filePaths = new ArrayList<>();

        if (!CollectionUtils.isEmpty(event.getEventFiles())) {
            List<FileDTO> files;
            try {
                files = getFiles(event.getEventFiles().stream()
                        .map(EventFile::getFileId).collect(Collectors.toList()));
            } catch (ResponseException e) {
                failEvent(event, localeStringService.getMessage("FILE_ATTACHMENT_NOT_FOUND", ""));
                return;
            }
            log.info("Start send event {}, download files {}", event.getId(), files);

            for (FileDTO file : files) {
                ResponseEntity<byte[]> responseEntity = null;
                try {
                    responseEntity = storageClient.downloadFileById(file.getId());
                    if (responseEntity.getBody() == null) {
                        continue;
                    }
                } catch (ResponseException e) {
                    failEvent(event, localeStringService.getMessage("FILE_ATTACHMENT_CAN_NOT_DOWNLOAD", ""));
                    return;
                }
                try {
                    InputStream inputStream = new ByteArrayInputStream(responseEntity.getBody());
                    Path path = Paths.get(folderTemporary + Const.SLASH + file.getId());
                    Files.deleteIfExists(path);
                    if (Files.notExists(Paths.get(folderTemporary))) {
                        log.warn("Need create folder: {}", path);
                        if (!(new File(Paths.get(folderTemporary).toString()).mkdirs())) {
                            log.warn("Create Folder Fail: {}", path);
                        }
                    }
                    Files.copy(inputStream, path);
                    filePaths.add(path.toString());
                    fileNames.add(file.getOriginalName());

                } catch (Exception e) {
                    log.warn("Save attach file fail", e);
                }
            }
        } else {
            log.info("Send event {}, don't need download files", event.getId());
        }
        String logoPath = null;
        if (event.getContent().contains("cid:" + Const.LOGO_CONTENT_ID)) {
            logoPath = Const.LOGO_PATH;
        }
        try {
            log.info("Send email of event {}, starting", event.getId());
            mailService.sendAttachmentsMailBcc(emailUsers, event.getTitle(), event.getContent(), filePaths, fileNames, logoPath, Const.LOGO_CONTENT_ID);
            log.info("Send email of event {}, success", event.getId());
        } catch (MessagingException e) {
            log.warn("Send attachments mail fail", e);
        }
        log.info("Send email of event {}, delete file", event.getId());
        filePaths.forEach(filePath -> {
            try {
                cleanUp(Path.of(filePath));
            } catch (IOException e) {
                log.warn("Delete file fail: {}", filePath);
            }
        });
    }

    private void cleanUp(Path path) throws IOException {
        Files.delete(path);
    }

    @Override
    public void sendViaFirebase(Event event) {
        log.info("Start send firebase by eventId: {}", event.getId());
        Map<String, String> data = parseDataToMap(event);
        List<String> userIds = event.getNotifications().stream()
                .map(Notification::getUserId)
                .distinct()
                .collect(Collectors.toList());

        List<DeviceEntity> devices = deviceRepository.getDeviceEntitiesByUserIds(userIds);
        FirebaseData firebaseData = FirebaseData.builder()
                .title(event.getTitle())
                .body(!StrUtils.isBlank(event.getDescription()) ? event.getDescription() : "")
                .data(data)
                .build();
        if (CollectionUtils.isEmpty(devices)) {
            log.info("Don't have device to receive notification of event {}", event.getId());
        }
        devices.forEach(d -> {
            Optional<Notification> optionalNotification = event.getNotifications().stream().
                    filter(notification -> Objects.equals(notification.getUserId(), d.getUserId())).
                    findFirst();
            if (optionalNotification.isPresent()) {
                Notification it = optionalNotification.get();
                log.info("Send notification {} to user {} on device {}", it.getId(), d.getUserId(), d.getId());
                firebaseData.getData().put("id", it.getId());
                fcmService.sendToDevice(firebaseData, d.getDeviceToken());
            }
        });
    }

    @Override
    public void sendViaTelegram(Event event) {
        log.info("Start send telegram by eventId: {}", event.getId());
        List<String> userIds = event.getNotifications().stream()
                .map(Notification::getUserId)
                .distinct()
                .collect(Collectors.toList());
        List<UserTelegramEntity> userTelegrams = userTelegramRepository.findByUserIds(userIds);
        userTelegrams.forEach(it -> telegramBotService.sendMessage(it.getChatId(), event.getContent()));
    }

    @Override
    public void sendViaSms(Event event) {
       //null
    }

    @Override
    @Transactional
    public void sendNotification(String eventId) {
        Optional<EventEntity> eventEntity = eventRepository.findByIdActivated(eventId);
        if (eventEntity.isEmpty()) {
            log.info("Send event {}, not found", eventId);
            return;
        }
        Event event = eventEntityMapper.toDomain(eventEntity.get());
        if (!EventStatus.IN_PROGRESS.equals(event.getStatus())) {
            log.info("Cant send email for event: {}, status is not in progress", event);
            return;
        }
        enrichEvent(event);
        event.sent();
        if (CollectionUtils.isEmpty(event.getTypes())) {
            return;
        }
        if (event.getTypes().contains(EventType.NOTIFICATION)) {
            this.sendViaFirebase(event);
            this.sendViaTelegram(event);
        }
        if (event.getTypes().contains(EventType.EMAIL)) {
            log.info("Start send email by eventId: {}", event.getId());
            this.sendViaEmail(event);
        }
        log.info("Send event {} success", eventId);
        save(event);
    }

    private Map<String, String> parseDataToMap(Event event) {
        Map<String, String> data = new HashMap<>();
        data.put("title", event.getTitle());
        data.put("content", Objects.equals(event.getContentType(), ContentType.JSON) ? event.getContent() : "");
        data.put("contentType", event.getContentType().name());
        data.put("description", Objects.nonNull(event.getDescription()) ? event.getDescription() : "");
        data.put("eventId", event.getId());
        data.put("id", "");
        data.put("effect", Objects.nonNull(event.getEffectType()) ? event.getEffectType().name() : "");
        return data;
    }

    private List<FileDTO> getFiles(List<String> fileIds) {
        List<FileDTO> files = new ArrayList<>();
        if (!CollectionUtils.isEmpty(fileIds)) {
            Response<List<FileDTO>> responseFiles = storageClient.findFileByIds(new FindByIdsRequest(fileIds));
            if (responseFiles.isSuccess() && Objects.nonNull(responseFiles.getData())) {
                files = responseFiles.getData();
            }
        }
        return files;
    }

    void enrichEvent(Event event) {
        List<Notification> notifications = notificationRepository.findAllByEventId(event.getId()).stream()
                .map(notificationEntityMapper::toDomain).collect(Collectors.toList());
        event.enrichNotification(notifications);
        List<EventFile> eventFiles = eventFileRepository.findAllByEventId(event.getId()).stream()
                .map(eventFileEntityMapper::toDomain).collect(Collectors.toList());
        event.enrichEventFile(eventFiles);
        List<EventTarget> eventTargets = eventTargetRepository.findByEventId(event.getId()).stream()
                .map(eventTargetEntityMapper::toDomain).collect(Collectors.toList());
        event.enrichEventTargets(eventTargets);

        List<String> emails = new ArrayList<>();
        List<String> userTargetIds = getTargetIds(event.getEventTargets(), TargetType.USER);
        if (!CollectionUtils.isEmpty(event.getTypes()) && event.getTypes().contains(EventType.EMAIL)) {
            emails = getTargetIds(event.getEventTargets(), TargetType.EMAIL);
        }
        List<UserDTO> users = getUsers(userTargetIds);
        List<Target> targetUser = users.stream().map(it -> Target.builder().userId(it.getId()).email(it.getEmail()).build()).collect(Collectors.toList());
        List<Target> targetEmail = emails.stream().map(it -> Target.builder().email(it).build()).collect(Collectors.toList());
        List<Target> targets = Stream.of(targetUser, targetEmail).flatMap(Collection::stream).collect(Collectors.toList());
        event.enrichTargets(targets);
    }

    private List<String> getTargetIds(List<EventTarget> eventTargets, TargetType targetType) {
        if (CollectionUtils.isEmpty(eventTargets)) {
            return new ArrayList<>();
        }
        return eventTargets.stream()
                .filter(it -> Objects.equals(it.getTargetType(), targetType))
                .map(EventTarget::getTarget)
                .distinct()
                .collect(Collectors.toList());
    }

    private List<UserDTO> getUsers(List<String> userIds) {
        List<UserDTO> users = new ArrayList<>();
        if (!CollectionUtils.isEmpty(userIds)) {
            Response<List<UserDTO>> responseUsers = iamClient.findByUserIds(new FindByIdsRequest(userIds));
            if (responseUsers.isSuccess() && Objects.nonNull(responseUsers.getData())) {
                users = responseUsers.getData();
            }
        }
        return users;
    }

    private void failEvent(Event event, String failureCauses) {
        event.failed(failureCauses);
        eventRepository.save(eventEntityMapper.toEntity(event));
    }

    @Transactional
    public void save(Event event) {
        EventEntity eventEntity = eventEntityMapper.toEntity(event);
        this.eventRepository.save(eventEntity);
        List<Notification> notifications = event.getNotifications();
        if (!CollectionUtils.isEmpty(notifications)) {
            notificationRepository.saveAll(notificationEntityMapper.toEntity(notifications));
        }
    }
}
