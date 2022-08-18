package com.evotek.notification.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.dto.response.iam.DepartmentDTO;
import com.evotek.common.dto.response.iam.UserDTO;
import com.evotek.common.enums.ContentType;
import com.evotek.common.enums.EventType;
import com.evotek.common.enums.TargetType;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.IdUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.notification.domain.command.EventCreateCommand;
import com.evotek.notification.domain.command.EventUpdateCommand;
import com.evotek.notification.domain.command.IssueEventCmd;
import com.evotek.notification.infrastructure.support.enums.*;
import com.evotek.notification.infrastructure.support.exception.BadRequestError;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.springframework.util.CollectionUtils;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class Event extends AuditableDomain {

    private String id;
    private String description;
    private String title;
    private EventEffectType effectType;
    private EventStatus status;
    private EventSource eventSource;
    private Boolean deleted;
    private String content;
    private ContentType contentType;
    private List<EventFile> eventFiles;
    private String note;
    private String issuedUserId;
    private String issuedUserName;
    private String senderUserId;
    private String senderUsername;
    private Instant expectedNotificationAt; // ngày dự kiến gửi
    private Instant notificationAt; // ngày gửi thực tế
    private Set<EventType> types;
    private List<String> userIds;
    private List<String> departmentIds;
    private String failureCauses;
    private EventSendTo sendTo;

    private List<UserDTO> users;
    private List<DepartmentDTO> departments;

    @JsonIgnore
    private List<Target> targets;

    @JsonIgnore
    private List<Notification> notifications;

    @JsonIgnore
    private List<EventTarget> eventTargets;

    public Event(EventCreateCommand cmd) {
        this.id = IdUtils.nextId();
        this.title = cmd.getTitle();
        this.content = cmd.getContent();
        this.contentType = ContentType.HTML;
        this.description = cmd.getDescription();
        this.deleted = false;
        this.status = EventStatus.PENDING;
        this.effectType = cmd.getEffectType();
        this.sendTo = cmd.getSendTo();
        this.note = cmd.getNote();
        if (!Objects.isNull(cmd.getExpectedNotificationAt())
                && cmd.getExpectedNotificationAt().isBefore(Instant.now())) {
            throw new ResponseException(BadRequestError.EVENT_CAN_NOT_SEND_BEFORE_PRESENT_TIME);
        }
        this.expectedNotificationAt = cmd.getExpectedNotificationAt();
        this.eventSource = EventSource.USER;
        this.issuedUserId = cmd.getIssuedUserId();
        updateNewEventFile(cmd.getFileIds());
        updateNewTargets(cmd.getUserIds(), cmd.getDepartmentIds());
        this.types = new HashSet<>();
        if (!CollectionUtils.isEmpty(cmd.getTypes())) {
            this.types.addAll(cmd.getTypes());
        }
    }

    public Event(IssueEventCmd cmd) {
        this.id = IdUtils.nextId();
        this.notificationAt = Instant.now();
        this.status = EventStatus.IN_PROGRESS;
        this.eventSource = EventSource.SYSTEM;
        this.title = cmd.getTitle();
        this.content = cmd.getContent();
        if (Objects.isNull(cmd.getContentType())) {
            this.contentType = ContentType.HTML; //default html
        } else {
            this.contentType = cmd.getContentType();
        }
        this.deleted = false;
        this.types = new HashSet<>();
        if (!CollectionUtils.isEmpty(cmd.getTypes())) {
            this.types.addAll(cmd.getTypes());
        }
        this.eventTargets = new ArrayList<>();
        this.notifications = new ArrayList<>();
        if (!CollectionUtils.isEmpty(cmd.getTargets())) {
            List<String> tmpTargets = cmd.getTargets().stream().distinct().collect(Collectors.toList());
            if (this.types.contains(EventType.NOTIFICATION)) {
                tmpTargets.forEach(userId -> this.notifications.add(new Notification(userId, this.id)));
            }
            if (TargetType.EMAIL.equals(cmd.getTargetType())) {
                tmpTargets.forEach(email -> this.eventTargets.add(new EventTarget(this.id, email, TargetType.EMAIL)));
            } else if (TargetType.USER.equals(cmd.getTargetType())) {
                tmpTargets.forEach(userId -> this.eventTargets.add(new EventTarget(this.id, userId, TargetType.USER)));
            }
        }
        updateNewEventFile(cmd.getAttachmentFileIds());
    }

    public void update(EventUpdateCommand cmd) {
        // kiem tra trang thai hien tai cua event, chi cho phep update khi o trang thai waiting
        if (!EventStatus.PENDING.equals(this.status)) {
            throw new ResponseException(BadRequestError.EVENT_CAN_NOT_CHANGE);
        }
        this.content = !StrUtils.isBlank(cmd.getContent()) ? cmd.getContent() : this.content;
        this.description = !StrUtils.isBlank(cmd.getDescription()) ? cmd.getDescription() : this.description;
        this.title = !StrUtils.isBlank(cmd.getTitle()) ? cmd.getTitle() : this.title;
        this.note = cmd.getNote();

        if (!Objects.isNull(cmd.getExpectedNotificationAt())
                && cmd.getExpectedNotificationAt().isBefore(Instant.now())) {
            throw new ResponseException(BadRequestError.EVENT_CAN_NOT_SEND_BEFORE_PRESENT_TIME);
        }
        this.expectedNotificationAt = cmd.getExpectedNotificationAt();
        updateNewEventFile(cmd.getFileIds());
        updateNewTargets(cmd.getUserIds(), cmd.getDepartmentIds());
    }

    public void delete() {
        if (Objects.equals(this.status, EventStatus.DONE) || Objects.equals(this.status, EventStatus.IN_PROGRESS)) {
            throw new ResponseException(BadRequestError.EVENT_CAN_NOT_DELETE);
        }
        this.deleted = true;

        if (!CollectionUtils.isEmpty(this.eventTargets)) {
            this.eventTargets.forEach(EventTarget::delete);
        }
        if (!CollectionUtils.isEmpty(this.eventFiles)) {
            this.eventFiles.forEach(EventFile::delete);
        }
    }

    public void sent() {
        this.status = EventStatus.DONE;
        if (!CollectionUtils.isEmpty(this.notifications)) {
            this.notifications.forEach(Notification::sent);
        }
    }

    public void preSent(String currentUserId) {
        if (!CollectionUtils.isEmpty(this.types) && this.types.contains(EventType.NOTIFICATION)) {
            this.generateNotification();
        }
        if (Objects.isNull(this.notificationAt)) {
            this.notificationAt = Instant.now();
        }
        this.senderUserId = currentUserId;
        this.status = EventStatus.IN_PROGRESS;
    }

    public void failed(String failureCauses) {
        this.status = EventStatus.FAILED;
        this.failureCauses = failureCauses;
    }

    public void cancel() {
        // kiem tra trang thai cua event, neu dang o trang thai WAITING thi cho phep huy, nguoc lai thi khong
        if (!status.equals(EventStatus.PENDING)) {
            throw new ResponseException(BadRequestError.EVENT_CAN_NOT_CANCEL);
        }
        this.status = EventStatus.CANCELLED;
        if (this.eventFiles != null && !this.eventFiles.isEmpty()) {
            this.eventFiles.forEach(EventFile::delete);
        }

        if (this.notifications != null && !this.notifications.isEmpty()) {
            this.notifications.forEach(Notification::deleted);
        }
    }

    public void generateNotification() {
        if (Objects.nonNull(this.targets)) {
            List<String> targetUserIds = this.targets.stream().map(Target::getUserId).collect(Collectors.toList());
            List<Notification> notificationList = new ArrayList<>();
            targetUserIds.forEach(target -> {
                Notification notification = new Notification(target, id);
                notificationList.add(notification);
            });
            this.notifications = notificationList;
        }
    }

    public void enrichEventFile(List<EventFile> eventFiles) {
        this.eventFiles = eventFiles;
    }

    public void enrichNotification(List<Notification> notifications) {
        this.notifications = notifications;
    }

    public void enrichTargets(List<Target> targets) {
        this.targets = targets;
    }

    public void enrichEventTargets(List<EventTarget> eventTargets) {
        this.eventTargets = eventTargets;
        if (!CollectionUtils.isEmpty(this.eventTargets)) {
            this.userIds = this.eventTargets.stream()
                    .filter(it -> TargetType.USER.equals(it.getTargetType()))
                    .map(EventTarget::getTarget)
                    .collect(Collectors.toList());
            this.departmentIds = this.eventTargets.stream()
                    .filter(it -> TargetType.DEPARTMENT.equals(it.getTargetType()))
                    .map(EventTarget::getTarget)
                    .collect(Collectors.toList());
        }
    }

    private void updateNewTargets(List<String> userIds, List<String> departmentIds) {
        if (CollectionUtils.isEmpty(this.eventTargets)) {
            this.eventTargets = new ArrayList<>();
        }
        this.eventTargets.forEach(EventTarget::delete);
        if (!CollectionUtils.isEmpty(userIds)) {
            for (String userId : userIds) {
                Optional<EventTarget> eventTargetOptional = this.eventTargets.stream()
                        .filter(it -> Objects.equals(userId, it.getTarget()) && TargetType.USER.equals(it.getTargetType()))
                        .findFirst();
                if (eventTargetOptional.isEmpty()) {
                    this.eventTargets.add(new EventTarget(this.id, userId, TargetType.USER));
                } else {
                    eventTargetOptional.get().unDeleted();
                }
            }
        }
        if (!CollectionUtils.isEmpty(departmentIds)) {
            for (String departmentId : departmentIds) {
                Optional<EventTarget> eventTargetOptional = this.eventTargets.stream()
                        .filter(it -> Objects.equals(departmentId, it.getTarget()) && TargetType.DEPARTMENT.equals(it.getTargetType()))
                        .findFirst();
                if (eventTargetOptional.isEmpty()) {
                    this.eventTargets.add(new EventTarget(this.id, departmentId, TargetType.DEPARTMENT));
                } else {
                    eventTargetOptional.get().unDeleted();
                }
            }
        }
    }

    private void updateNewEventFile(List<String> fileIds) {
        if (CollectionUtils.isEmpty(this.eventFiles)) {
            this.eventFiles = new ArrayList<>();
        }
        this.eventFiles.forEach(EventFile::delete);
        if (!CollectionUtils.isEmpty(fileIds)) {
            for (String fileId : fileIds) {
                Optional<EventFile> eventFileOptional = this.eventFiles.stream()
                        .filter(ef -> ef.getFileId().equals(fileId)).findFirst();
                if (eventFileOptional.isEmpty()) {
                    this.eventFiles.add(new EventFile(fileId, id, EventFileType.ATTACHMENT));
                } else {
                    EventFile eventFile = eventFileOptional.get();
                    eventFile.unDelete();
                }
            }
        }
    }

    public void enrichIssuedUserName(String name) {
        this.issuedUserName = name;
    }

    public void enrichSenderUserName(String senderUsername) {
        this.senderUsername = senderUsername;
    }

    public void enrichTargetDepartmentsAndUsers(List<DepartmentDTO> departments, List<UserDTO> users) {
        this.departments = departments;
        this.users = users;
    }

}
