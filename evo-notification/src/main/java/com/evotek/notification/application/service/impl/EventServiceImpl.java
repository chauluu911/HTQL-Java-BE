package com.evotek.notification.application.service.impl;

import com.evotek.common.client.iam.IAMClient;
import com.evotek.common.client.storage.StorageClient;
import com.evotek.common.dto.PageDTO;
import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.request.notification.IssueEventRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.iam.DepartmentDTO;
import com.evotek.common.dto.response.iam.UserDTO;
import com.evotek.common.dto.response.storage.FileDTO;
import com.evotek.common.enums.TargetType;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.StrUtils;
import com.evotek.common.webapp.support.AbstractDomainService;
import com.evotek.common.webapp.support.SecurityUtils;
import com.evotek.notification.application.dto.request.EventCreateRequest;
import com.evotek.notification.application.dto.request.EventSearchRequest;
import com.evotek.notification.application.dto.request.EventUpdateRequest;
import com.evotek.notification.application.mapper.*;
import com.evotek.notification.application.service.EventService;
import com.evotek.notification.domain.*;
import com.evotek.notification.domain.command.EventCreateCommand;
import com.evotek.notification.domain.command.EventUpdateCommand;
import com.evotek.notification.domain.command.IssueEventCmd;
import com.evotek.notification.domain.query.EventSearchQuery;
import com.evotek.notification.infrastructure.persistence.entity.EventEntity;
import com.evotek.notification.infrastructure.persistence.entity.EventFileEntity;
import com.evotek.notification.infrastructure.persistence.entity.EventTargetEntity;
import com.evotek.notification.infrastructure.persistence.repository.EventFileRepository;
import com.evotek.notification.infrastructure.persistence.repository.EventRepository;
import com.evotek.notification.infrastructure.persistence.repository.EventTargetRepository;
import com.evotek.notification.infrastructure.persistence.repository.NotificationRepository;
import com.evotek.notification.infrastructure.support.enums.EventSendTo;
import com.evotek.notification.infrastructure.support.enums.EventStatus;
import com.evotek.notification.infrastructure.support.exception.BadRequestError;
import com.evotek.notification.infrastructure.support.exception.NotFoundError;
import com.evotek.notification.infrastructure.support.util.Const;
import com.evotek.notification.infrastructure.support.util.JobConst;
import com.github.kagkarlsson.scheduler.Scheduler;
import com.github.kagkarlsson.scheduler.task.Task;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
@Slf4j
public class EventServiceImpl extends AbstractDomainService<Event, EventEntity, String> implements EventService {

    private final AutoMapper autoMapper;
    private final EventEntityMapper eventEntityMapper;
    private final EventRepository eventRepository;
    private final NotificationRepository notificationRepository;
    private final NotificationEntityMapper notificationEntityMapper;
    private final EventFileRepository eventFileRepository;
    private final EventFileEntityMapper eventFileEntityMapper;
    private final EventTargetRepository eventTargetRepository;
    private final EventTargetEntityMapper eventTargetEntityMapper;
    private final StorageClient storageClient;
    private final IAMClient iamClient;
    private final AutoMapperQuery autoMapperQuery;
    private final Scheduler scheduler;
    private final Task<String> manuallySendEventTask;

    public EventServiceImpl(EventEntityMapper eventEntityMapper,
                            EventRepository eventRepository,
                            NotificationRepository notificationRepository,
                            NotificationEntityMapper notificationEntityMapper,
                            EventFileRepository eventFileRepository,
                            EventFileEntityMapper eventFileEntityMapper,
                            StorageClient storageClient,
                            AutoMapper autoMapper,
                            EventTargetRepository eventTargetRepository,
                            EventTargetEntityMapper eventTargetEntityMapper,
                            IAMClient iamClient,
                            AutoMapperQuery autoMapperQuery,
                            Scheduler scheduler,
                            Task<String> manuallySendEventTask) {
        super(eventRepository, eventEntityMapper);
        this.autoMapper = autoMapper;
        this.eventEntityMapper = eventEntityMapper;
        this.eventRepository = eventRepository;
        this.notificationRepository = notificationRepository;
        this.notificationEntityMapper = notificationEntityMapper;
        this.eventFileRepository = eventFileRepository;
        this.eventFileEntityMapper = eventFileEntityMapper;
        this.storageClient = storageClient;
        this.eventTargetRepository = eventTargetRepository;
        this.eventTargetEntityMapper = eventTargetEntityMapper;
        this.iamClient = iamClient;
        this.autoMapperQuery = autoMapperQuery;
        this.scheduler = scheduler;
        this.manuallySendEventTask = manuallySendEventTask;
    }

    @Override
    @Transactional
    public Event create(EventCreateRequest request) {
        EventCreateCommand cmd = autoMapper.from(request);
        checkExistFiles(cmd.getFileIds());
        cmd.setIssuedUserId(currentUserId());
        if (EventSendTo.DEPARTMENT.equals(cmd.getSendTo()) && CollectionUtils.isEmpty(cmd.getDepartmentIds())) {
            log.info("Create event with type all department and send to department");
            List<DepartmentDTO> departments = getAllDepartments();
            cmd.setDepartmentIds(departments.stream().map(DepartmentDTO::getId).collect(Collectors.toList()));
        } else if (EventSendTo.USER.equals(cmd.getSendTo()) && CollectionUtils.isEmpty(cmd.getUserIds())) {
            log.info("Create event with type all user and send to user");
            List<UserDTO> users = iamClient.getAllUser();
            cmd.setUserIds(users.stream().map(UserDTO::getId).collect(Collectors.toList()));
        }
        Event event = new Event(cmd);
        save(event);
        return event;
    }

    @Override
    @Transactional
    public Event update(String uuid, EventUpdateRequest request) {
        Event event = ensureExisted(uuid);
        enrichEvent(event);
        EventUpdateCommand cmd = autoMapper.from(request);

        checkExistFiles(cmd.getFileIds());
        event.update(cmd);
        save(event);
        return event;
    }

    @Override
    public Event getById(String id) {
        EventEntity eventEntity = eventRepository.findByIdActivated(id).orElseThrow(() ->
                new ResponseException(BadRequestError.EVENT_NOT_EXISTED)
        );
        Optional<Event> eventOpt = enrichEvents(Collections.singletonList(eventEntity)).stream().findFirst();
        eventOpt.ifPresent(event -> {
            List<UserDTO> users = getUsers(event.getUserIds());
            List<DepartmentDTO> departments = getDepartmentByIds(event.getDepartmentIds());
            event.enrichTargetDepartmentsAndUsers(departments, users);
        });
        return eventOpt.orElse(null);
    }

    @Override
    public PageDTO<Event> search(EventSearchRequest request) {
        EventSearchQuery searchQuery = autoMapperQuery.toQuery(request);
        List<EventEntity> entities = eventRepository.search(searchQuery);
        List<Event> events = enrichEvents(entities);
        return new PageDTO<>(events, searchQuery.getPageIndex(), searchQuery.getPageSize(), eventRepository.countEvent(searchQuery));
    }

    @Override
    @Transactional
    public void cancel(String uuid) {
        Event event = ensureExisted(uuid);
        enrichEvent(event);
        event.cancel();
        save(event);
    }

    @Override
    @Transactional
    public void send(String id) {
        Event event = ensureExisted(id);
        if (!Objects.equals(event.getStatus(), EventStatus.PENDING)) {
            throw new ResponseException(BadRequestError.EVENT_CAN_NOT_CHANGE);
        }
        enrichEventTargets(event);

        List<UserDTO> userTargets = new ArrayList<>();
        if (EventSendTo.ALL_USER.equals(event.getSendTo())) {
            userTargets = iamClient.getAllUser();
        } else if (EventSendTo.USER.equals(event.getSendTo())) {
            List<String> userIds = getTargetIds(event.getEventTargets(), TargetType.USER);
            userTargets = getUsers(userIds);
        } else if (EventSendTo.DEPARTMENT.equals(event.getSendTo())) {
            List<String> departIds = getTargetIds(event.getEventTargets(), TargetType.DEPARTMENT);
            userTargets = getUsersByDepartmentIds(departIds);
        }
        List<Target> targets = userTargets.stream().map(it -> new Target(it.getId(), it.getEmail())).collect(Collectors.toList());
        event.enrichTargets(targets);
        List<EventFileEntity> eventFileEntities = eventFileRepository.findAllByEventId(event.getId());
        List<EventFile> eventFiles = eventFileEntityMapper.toDomain(eventFileEntities);
        event.enrichEventFile(eventFiles);
        event.preSent(currentUserId());
        save(event);
        scheduler.schedule(this.manuallySendEventTask.instance(UUID.randomUUID().toString(), event.getId()),
                Instant.now().plusSeconds(JobConst.SEND_DELAY));
        log.info("Send event {}, create job", id);
    }

    private List<String> getTargetIds(List<EventTarget> eventTargets, TargetType targetType) {
        if (CollectionUtils.isEmpty(eventTargets)) {
            return new ArrayList<>();
        }
        return eventTargets.stream()
                .filter(it -> Objects.equals(targetType, it.getTargetType()))
                .map(EventTarget::getTarget)
                .collect(Collectors.toList());
    }

    @Override
    @Transactional
    public void delete(String id) {
        Event event = ensureExisted(id);
        enrichEvent(event);
        event.delete();
        save(event);
    }

    @Override
    @Transactional
    public Event issued(IssueEventRequest request) {
        log.info("Issue event by system: {}", request);
        IssueEventCmd cmd = autoMapper.from(request);
        Event event = new Event(cmd);
        save(event);
        scheduler.schedule(this.manuallySendEventTask.instance(UUID.randomUUID().toString(), event.getId()),
                Instant.now().plusSeconds(JobConst.SEND_DELAY));
        log.info("Send system event {}, create job", event.getId());
        return event;
    }

    @Override
    @Transactional
    public Event save(Event event) {
        EventEntity eventEntity = eventEntityMapper.toEntity(event);
        this.eventRepository.save(eventEntity);
        List<Notification> notifications = event.getNotifications();
        if (!CollectionUtils.isEmpty(notifications)) {
            notificationRepository.saveAll(notificationEntityMapper.toEntity(notifications));
        }
        List<EventFile> eventFiles = event.getEventFiles();
        if (!CollectionUtils.isEmpty(eventFiles)) {
            eventFileRepository.saveAll(eventFileEntityMapper.toEntity(eventFiles));
        }
        List<EventTarget> eventTargets = event.getEventTargets();
        if (!CollectionUtils.isEmpty(eventTargets)) {
            eventTargetRepository.saveAll(eventTargetEntityMapper.toEntity(eventTargets));
        }
        return event;
    }

    Event ensureExisted(String id) {
        return findById(id).orElseThrow(() ->
                new ResponseException(BadRequestError.EVENT_NOT_EXISTED));
    }

    private void enrichEventTargets(Event event) {
        List<EventTargetEntity> eventTargetEntities = eventTargetRepository.findByEventId(event.getId());
        List<EventTarget> eventTargets = eventTargetEntities.stream().map(eventTargetEntityMapper::toDomain).collect(Collectors.toList());
        event.enrichEventTargets(eventTargets);
    }

    void enrichEvent(Event event) {
        List<EventFile> eventFiles = eventFileRepository.findAllByEventId(event.getId()).stream()
                .map(eventFileEntityMapper::toDomain).collect(Collectors.toList());
        event.enrichEventFile(eventFiles);
        List<Notification> notifications = notificationRepository.findAllByEventId(event.getId()).stream()
                .map(notificationEntityMapper::toDomain).collect(Collectors.toList());
        event.enrichNotification(notifications);
        List<EventTarget> eventTargets = eventTargetRepository.findByEventId(event.getId()).stream()
                .map(eventTargetEntityMapper::toDomain).collect(Collectors.toList());
        event.enrichEventTargets(eventTargets);
    }

    private List<Event> enrichEvents(List<EventEntity> eventEntities) {
        List<Event> events = eventEntityMapper.toDomain(eventEntities);
        List<String> eventIds = eventEntities.stream().map(EventEntity::getId).collect(Collectors.toList());

        List<String> issuedUserIds = eventEntities.stream()
                .map(EventEntity::getIssuedUserId).distinct().filter(Objects::nonNull).collect(Collectors.toList());
        List<String> senderUserIds = eventEntities.stream()
                .map(EventEntity::getSenderUserId).distinct().filter(Objects::nonNull).collect(Collectors.toList());
        List<String> userIds = Stream.of(issuedUserIds, senderUserIds)
                .flatMap(Collection::stream).distinct().collect(Collectors.toList());
        List<UserDTO> users = getUsers(userIds);

        List<EventFileEntity> eventFileEntities = eventFileRepository.findAllByEventIds(eventIds);
        List<EventFile> eventFiles = eventFileEntityMapper.toDomain(eventFileEntities);
        enrichDetailEventFile(eventFiles);

        List<EventTargetEntity> eventTargetEntities = eventTargetRepository.findByEventIds(eventIds);
        enrichEventTarget(events, eventTargetEntityMapper.toDomain(eventTargetEntities));

        for (Event event : events) {
            if (!CollectionUtils.isEmpty(eventFiles)) {
                List<EventFile> evtEventFiles = eventFiles.stream().
                        filter(eventFile -> Objects.equals(event.getId(), eventFile.getEventId())).
                        collect(Collectors.toList());
                event.enrichEventFile(evtEventFiles);
            }
            if (!CollectionUtils.isEmpty(users)) {
                Optional<UserDTO> optionalIssuedUser = users.stream()
                        .filter(it -> it.getId().equals(event.getIssuedUserId())).findFirst();
                optionalIssuedUser.ifPresent(user -> event.enrichIssuedUserName(user.getFullName()
                        + (StrUtils.isBlank(user.getTitle()) ? "" : ("-" + user.getTitle()))));

                Optional<UserDTO> optionalSenderUser = users.stream()
                        .filter(it -> it.getId().equals(event.getSenderUserId())).findFirst();
                optionalSenderUser.ifPresent(user -> event.enrichSenderUserName(user.getFullName()
                        + (StrUtils.isBlank(user.getTitle()) ? "" : ("-" + user.getTitle()))));
            }
        }
        return events;
    }

    private void enrichDetailEventFile(List<EventFile> eventFiles) {
        List<String> fileIds = eventFiles.stream().map(EventFile::getFileId).collect(Collectors.toList());
        List<FileDTO> files = getFiles(fileIds);
        for (EventFile eventFile : eventFiles) {
            Optional<FileDTO> file = files.stream()
                    .filter(it -> Objects.equals(it.getId(), eventFile.getFileId())).findFirst();
            file.ifPresent(it -> eventFile.enrichFile(it.getOriginalName(), it.getViewUrl(), it.getDownloadUrl()));
        }
    }

    private void enrichEventTarget(List<Event> events, List<EventTarget> eventTargets) {
        for (Event event : events) {
            List<EventTarget> targets = eventTargets.stream()
                    .filter(eventTarget -> Objects.equals(eventTarget.getEventId(), event.getId()))
                    .collect(Collectors.toList());
            event.enrichEventTargets(targets);
        }
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

    private List<UserDTO> getUsersByDepartmentIds(List<String> departmentIds) {
        List<UserDTO> users = new ArrayList<>();
        if (!CollectionUtils.isEmpty(departmentIds)) {
            Response<List<UserDTO>> responseUsers = iamClient.findByDepartmentIds(new FindByIdsRequest(departmentIds));
            if (responseUsers.isSuccess() && Objects.nonNull(responseUsers.getData())) {
                users = responseUsers.getData();
            }
        }
        return users;
    }

    private List<DepartmentDTO> getAllDepartments() {
        List<DepartmentDTO> departments = new ArrayList<>();
        Response<List<DepartmentDTO>> response = iamClient.getAllDepartment();
        if (response.isSuccess() && Objects.nonNull(response.getData())) {
            departments = response.getData();
        }
        return departments;
    }

    private List<DepartmentDTO> getDepartmentByIds(List<String> ids) {
        List<DepartmentDTO> departments = new ArrayList<>();
        if (!CollectionUtils.isEmpty(ids)) {
            Response<List<DepartmentDTO>> response = iamClient.getDepartmentByIds(FindByIdsRequest.builder().ids(ids).build());
            if (response.isSuccess() && Objects.nonNull(response.getData())) {
                departments = response.getData();
            }
        }
        return departments;
    }

    private String currentUserId() {
        return SecurityUtils.getCurrentUserLoginId().orElseThrow(() ->
                new ResponseException(NotFoundError.USER_ID_IS_NULL));
    }

    public void checkExistFiles(List<String> fileIds) {
        if (CollectionUtils.isEmpty(fileIds)) {
            return;
        }
        Response<List<FileDTO>> response = storageClient.findFileByIds(new FindByIdsRequest(fileIds));
        if (response.isSuccess() && Objects.nonNull(response.getData())) {
            List<FileDTO> files = response.getData();
            if (!Objects.equals(fileIds.size(), files.size())) {
                throw new ResponseException(BadRequestError.FILE_NOT_EXIST);
            }
            for (FileDTO file : files) {
                if (!Const.WHITELIST_FILE_TYPES.contains(file.getType())) {
                    throw new ResponseException(BadRequestError.FILE_MUST_BE_PDF);
                }
            }
            return;
        }
        throw new ResponseException(BadRequestError.FILE_NOT_EXIST);
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

}
