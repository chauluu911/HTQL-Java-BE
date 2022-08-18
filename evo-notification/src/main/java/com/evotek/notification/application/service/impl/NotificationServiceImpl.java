package com.evotek.notification.application.service.impl;

import com.evotek.common.client.storage.StorageClient;
import com.evotek.common.dto.PageDTO;
import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.storage.FileDTO;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainService;
import com.evotek.common.webapp.support.SecurityUtils;
import com.evotek.notification.application.dto.request.NotificationDeleteRequest;
import com.evotek.notification.application.dto.request.NotificationMarkReadRequest;
import com.evotek.notification.application.dto.request.NotificationMarkUnreadRequest;
import com.evotek.notification.application.dto.request.NotificationSearchRequest;
import com.evotek.notification.application.mapper.AutoMapperQuery;
import com.evotek.notification.application.mapper.EventEntityMapper;
import com.evotek.notification.application.mapper.NotificationEntityMapper;
import com.evotek.notification.application.service.NotificationService;
import com.evotek.notification.domain.Event;
import com.evotek.notification.domain.Notification;
import com.evotek.notification.domain.query.NotificationSearchQuery;
import com.evotek.notification.infrastructure.persistence.entity.EventEntity;
import com.evotek.notification.infrastructure.persistence.entity.EventFileEntity;
import com.evotek.notification.infrastructure.persistence.entity.NotificationEntity;
import com.evotek.notification.infrastructure.persistence.repository.EventFileRepository;
import com.evotek.notification.infrastructure.persistence.repository.EventRepository;
import com.evotek.notification.infrastructure.persistence.repository.NotificationRepository;
import com.evotek.notification.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@Slf4j
public class NotificationServiceImpl extends AbstractDomainService<Notification, NotificationEntity, String> implements NotificationService {

    private final NotificationEntityMapper notificationEntityMapper;
    private final NotificationRepository notificationRepository;
    private final EventEntityMapper eventEntityMapper;
    private final EventRepository eventRepository;
    private final EventFileRepository eventFileRepository;
    private final StorageClient storageClient;
    private final AutoMapperQuery autoMapperQuery;

    @Value("${bot.username}")
    private String usernameTelegram;

    public NotificationServiceImpl(NotificationEntityMapper notificationEntityMapper,
                                   NotificationRepository notificationRepository,
                                   EventEntityMapper eventEntityMapper,
                                   EventRepository eventRepository,
                                   EventFileRepository eventFileRepository,
                                   StorageClient storageClient,
                                   AutoMapperQuery autoMapperQuery) {
        super(notificationRepository, notificationEntityMapper);
        this.notificationEntityMapper = notificationEntityMapper;
        this.notificationRepository = notificationRepository;
        this.eventEntityMapper = eventEntityMapper;
        this.eventRepository = eventRepository;
        this.eventFileRepository = eventFileRepository;
        this.storageClient = storageClient;
        this.autoMapperQuery = autoMapperQuery;
    }

    @Override
    public Notification ensureExisted(String uuid) {
        return findById(uuid).orElseThrow(() ->
                new ResponseException(NotFoundError.NOTIFICATION_NOT_FOUND));
    }

    @Override
    public Notification getNotificationByEventId(String eventId) {
        NotificationEntity notificationEntity = notificationRepository.findByUserIdAndEventId(this.currentUserId(), eventId)
                .orElseThrow(() -> new ResponseException(NotFoundError.NOTIFICATION_NOT_FOUND));
        Notification notification = this.enrichNotification(notificationEntity);
        if (Boolean.FALSE.equals(notification.getIsRead())) {
            notification.read();
            save(notification);
        }
        return notification;
    }

    @Override
    public Notification getNotificationById(String id) {
        String currentUserId = currentUserId();
        log.info("User {} read by id: {}", currentUserId, id);

        NotificationEntity notificationEntity = notificationRepository.findByIdAndUserId(id, currentUserId)
                .orElseThrow(() -> new ResponseException(NotFoundError.NOTIFICATION_NOT_FOUND));
        Notification notification = this.enrichNotification(notificationEntity);
        if (Boolean.FALSE.equals(notification.getIsRead())) {
            notification.read();
            save(notification);
        }
        return notification;
    }

    @Override
    public PageDTO<Notification> findAllByUserLogin(NotificationSearchRequest request) {
        NotificationSearchQuery params = autoMapperQuery.toQuery(request);
        params.setUserId(this.currentUserId());
        List<NotificationEntity> notificationEntities = notificationRepository.search(params);
        List<Notification> notifications = this.enrichNotifications(notificationEntities);
        return new PageDTO<>(notifications, params.getPageIndex(), params.getPageSize(), notificationRepository.count(params));
    }

    @Override
    public Boolean markAllRead(NotificationMarkReadRequest notificationMarkReadRequest) {
        log.info("Mark all read by ids: {}", notificationMarkReadRequest.getIds());
        List<Notification> notifications = this.findAllNotificationByIdsAndCurrentUserId(notificationMarkReadRequest.getIds());
        notifications.forEach(Notification::read);
        saveAll(notifications);
        return true;
    }

    @Override
    public Boolean markAllUnreadByIds(NotificationMarkUnreadRequest request) {
        log.info("Mark all unread by ids: {}", request.getIds());
        List<Notification> notifications = this.findAllNotificationByIdsAndCurrentUserId(request.getIds());
        notifications.forEach(Notification::unread);
        saveAll(notifications);
        return true;
    }

    @Override
    public Boolean markRead(String id) {
        String currentUserId = currentUserId();
        log.info("User {} mark read by id: {}", currentUserId, id);
        NotificationEntity notificationEntity = notificationRepository.findByIdAndUserId(id, currentUserId)
                .orElseThrow(() -> new ResponseException(NotFoundError.NOTIFICATION_NOT_FOUND));
        Notification notification = notificationEntityMapper.toDomain(notificationEntity);
        notification.read();
        save(notification);
        return true;
    }

    @Override
    public Boolean deleteAll(NotificationDeleteRequest request) {
        log.info("Delete notification ids {}", request.getIds());

        List<Notification> notifications = this.findAllNotificationByIdsAndCurrentUserId(request.getIds());
        notifications.forEach(Notification::deleted);
        saveAll(notifications);
        return true;
    }

    @Override
    public Boolean delete(String id) {
        String currentUserId = currentUserId();
        log.info("User {} delete notification id {}", currentUserId, id);

        NotificationEntity notificationEntity = notificationRepository.findByIdAndUserId(id, this.currentUserId())
                .orElseThrow(() -> new ResponseException(NotFoundError.NOTIFICATION_NOT_FOUND));
        Notification notification = notificationEntityMapper.toDomain(notificationEntity);
        notification.deleted();
        save(notification);
        return true;
    }

    @Override
    public Long countUnreadNotification() {
        return notificationRepository.countUnreadNotification(currentUserId());
    }

    @Override
    public Boolean markReadAll() {
        String currentUserId = currentUserId();
        log.info("Mark read all by user: {}", currentUserId);
        List<NotificationEntity> notificationEntities = notificationRepository.findAllUnreadNotiByUserId(currentUserId);
        if (CollectionUtils.isEmpty(notificationEntities)) {
            throw new ResponseException(NotFoundError.NOTIFICATION_NOT_FOUND);
        }
        List<Notification> notifications = notificationEntityMapper.toDomain(notificationEntities);
        notifications.forEach(Notification::read);
        saveAll(notifications);
        return true;
    }

    @Override
    public String getUrlJoinTelegramBot() {
        final String urlTemplate = "https://t.me/%s?start=%s";
        return String.format(urlTemplate, usernameTelegram, currentUserId());
    }

    private Notification enrichNotification(NotificationEntity notificationEntity) {
        Notification notification = notificationEntityMapper.toDomain(notificationEntity);
        Event event = eventEntityMapper.toDomain(eventRepository.findByIdActivated(notificationEntity.getEventId())
                .orElseThrow(() -> new ResponseException(NotFoundError.EVENT_NOT_FOUND)));

        List<EventFileEntity> eventFileEntities = eventFileRepository.findAllByEventId(event.getId());
        List<String> fileIds = eventFileEntities.stream().map(EventFileEntity::getFileId).collect(Collectors.toList());
        List<FileDTO> files = new ArrayList<>();

        if (!CollectionUtils.isEmpty(fileIds)) {
            Response<List<FileDTO>> responseFiles = storageClient.findFileByIds(new FindByIdsRequest(fileIds));
            if (responseFiles.isSuccess() && Objects.nonNull(responseFiles.getData())) {
                files = responseFiles.getData();
            }
        }
        notification.enrichEvent(event);
        notification.enrichAttachFiles(files);
        return notification;
    }

    private List<Notification> enrichNotifications(List<NotificationEntity> notificationEntities) {
        List<String> eventIds = notificationEntities.stream()
                .map(NotificationEntity::getEventId).distinct().collect(Collectors.toList());
        List<EventEntity> eventEntities = eventRepository.findAllByIds(eventIds);
        List<Event> events = eventEntityMapper.toDomain(eventEntities);

        List<Notification> notifications = notificationEntityMapper.toDomain(notificationEntities);
        notifications.forEach(notification -> {
            Optional<Event> event = events.stream()
                    .filter(it -> it.getId().equals(notification.getEventId())).findFirst();
            event.ifPresent(notification::enrichEvent);
        });
        return notifications;
    }

    private List<Notification> findAllNotificationByIdsAndCurrentUserId(List<String> ids) {
        List<NotificationEntity> notificationEntities = notificationRepository.findAllByIdsAndUserId(ids, this.currentUserId());
        if (CollectionUtils.isEmpty(notificationEntities)) {
            throw new ResponseException(NotFoundError.NOTIFICATION_NOT_FOUND);
        }
        return notificationEntityMapper.toDomain(notificationEntities);
    }

    private String currentUserId() {
        return SecurityUtils.getCurrentUserLoginId().orElseThrow(() ->
                new ResponseException(NotFoundError.USER_ID_IS_NULL));
    }

}
