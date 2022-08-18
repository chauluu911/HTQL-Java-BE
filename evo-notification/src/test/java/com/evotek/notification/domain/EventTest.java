package com.evotek.notification.domain;

import com.evotek.common.enums.ContentType;
import com.evotek.notification.domain.command.EventCreateCommand;
import com.evotek.notification.domain.command.EventUpdateCommand;
import com.evotek.notification.domain.command.IssueEventCmd;
import com.evotek.notification.infrastructure.support.enums.*;
import com.google.gson.Gson;
import com.evotek.common.enums.DataType;
import com.evotek.common.enums.EventType;
import com.evotek.common.enums.TargetType;
import com.evotek.common.util.IdUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

class EventTest {

    private static final String TITLE = "title";
    private static final List<String> BUILDING_IDS = List.of("2f34c7e4-206e-4f6a-a03b-c1328e856b7a");
    private static final List<String> FILE_IDS = List.of("2f34c7e4-206e-4f6a-a03b-c13218091998");
    private static final String EXPECTED_NOTIFICATION_PATTERN = "yyyy-MM-dd";
    private static final String EXPECTED_NOTIFICATION_TIME = "2022-09-18";
    private static final String CONTENT = "content";
    private static final String NOTE = "note";
    private static final String ISSUED_USER_ID = "3ac11327-1aba-49d2-a0b0-e48784a51a7d"; // = mb.amc.center
    private static final String DESCRIPTION = "description";
    private static final String ATTACHED_LINK = "https://dantri.com.vn/";
    private static final String REASON = "File error";

    //Constant Update
    private static final String DESCRIPTION_UPDATE = "description update";
    private static final String TITLE_UPDATE = "title update";
    private static final String ATTACHED_LINK_UPDATE = "attached link update";
    private static final String CONTENT_UPDATE = "content update";
    private static final String NOTE_UPDATE = "note update";
    private static final List<String> FILE_IDS_UPDATE = Arrays.asList(
            "a730d76b-b6a3-47b5-8371-9ddddf58c9b6",
            "56f7064e-0bc6-4f51-9551-80dfe6635f70"
    );
    private static final List<String> TARGET_IDS = Arrays.asList(
            "2f34c7e4-206e-4f6a-a03b-c1328e856b1a",
            "2f34c7e4-206e-4f6a-a03b-c1328e856b2a"
    );
    private static final List<String> FLOOR_IDS = Arrays.asList(
            "6a59e991-24cd-4e2b-a7fe-0f691cedabf9",
            "aa088c9d-4988-4c1b-8302-13353f23be6a"
    );
    private static final List<String> ORGANIZATION_IDS = Arrays.asList(
            "e12ed642-e510-4a7f-9d8e-279792f89867",
            "d3ab94ad-4b00-4a06-8bab-74bd074b8203"
    );
    private static final List<String> ORGANIZATION_IDS_UPDATE = Arrays.asList(
            "e12ed642-e510-4a7f-9d8e-279792f89867",
            "72adccd2-6af1-4391-9c8e-abe59a2f9194"
    );
    private static final List<String> FLOOR_IDS_UPDATE = Arrays.asList(
            "6a59e991-24cd-4e2b-a7fe-0f691cedabf9",
            "aa088c9d-4988-4c1b-8302-13353f23be6a"
    );
    private static final List<String> BUILDING_IDS_UPDATE = Collections.singletonList("2f34c7e4-206e-4f6a-a03b-c1328e856b7a");
    private static final String BLANK_STRING = "";
    private static final String TICKET_ID = "56d86ec1-9adc-4aad-8545-7dd78264c047";
    private static final String BUILDING_ID = "2f34c7e4-206e-4f6a-a03b-c1328e856b7a";
    private final String evtId = IdUtils.nextId();
    private final String currentUserId = IdUtils.nextId();

    private List<EventFile> eventFiles;
    private List<EventTarget> eventTargets;
    private List<Target> targets;
    private List<Notification> notifications;

    @BeforeEach
    void setUp() {
        eventFiles = List.of(generateEventFile(), generateEventFile());
        eventTargets = List.of(generateEventTarget(), generateEventTarget());
        targets = List.of(generateTarget(), generateTarget());
        notifications = List.of(generateNotification(), generateNotification());
    }

    Target generateTarget() {
        return Target.builder().userId(IdUtils.nextId()).email("test@gmail.com").build();
    }

    Notification generateNotification() {
        return Notification.builder()
                .content("Nội dung")
                .eventId(evtId)
                .deleted(false)
                .userId(IdUtils.nextId())
                .build();
    }

    EventFile generateEventFile() {
        return EventFile.builder()
                .id(IdUtils.nextId())
                .eventId(evtId)
                .fileId(IdUtils.nextId())
                .deleted(false)
                .build();
    }

    EventTarget generateEventTarget() {
        return EventTarget.builder()
                .id(IdUtils.nextId())
                .targetType(TargetType.DEPARTMENT)
                .eventId(evtId)
                .target(IdUtils.nextId())
                .deleted(false)
                .build();
    }

    @Test
    @DisplayName("Function test create event")
    public void testEvent() throws Exception {
        EventCreateCommand command = EventCreateCommand.builder()
                .description(DESCRIPTION)
                .title(TITLE)
                .effectType(EventEffectType.ONLY_DISPLAY)
                .targetType(null)
                .expectedNotificationAt(new SimpleDateFormat(EXPECTED_NOTIFICATION_PATTERN).parse(EXPECTED_NOTIFICATION_TIME).toInstant())
                .content(CONTENT)
                .note(NOTE)
                .fileIds(FILE_IDS)
                .issuedUserId(ISSUED_USER_ID)
                .build();
        Event event = new Event(command);
        assertEquals(TITLE, event.getTitle());
        assertEquals(CONTENT, event.getContent());
        assertEquals(ContentType.HTML, event.getContentType());
        assertEquals(DESCRIPTION, event.getDescription());
        Assertions.assertEquals(EventStatus.PENDING, event.getStatus());
        assertEquals(EventEffectType.ONLY_DISPLAY, event.getEffectType());
        assertEquals(NOTE, event.getNote());
        assertEquals(new SimpleDateFormat(EXPECTED_NOTIFICATION_PATTERN).parse(EXPECTED_NOTIFICATION_TIME).toInstant(), event.getExpectedNotificationAt());
        assertEquals(ISSUED_USER_ID, event.getIssuedUserId());

        // Test createNewEventFile
        assertTrue(event.getEventFiles().stream().allMatch(
                eventFile -> Objects.equals(eventFile.getEventId(), event.getId())
                        && FILE_IDS.contains(eventFile.getFileId())));
    }

    @Test
    @DisplayName("Function test create event with customer scope")
    public void testEventWithCustomer() throws Exception {
        EventCreateCommand command = EventCreateCommand.builder()
                .description(DESCRIPTION)
                .title(TITLE)
                .effectType(EventEffectType.ONLY_DISPLAY)
                .targetType(null)
                .expectedNotificationAt(new SimpleDateFormat(EXPECTED_NOTIFICATION_PATTERN).parse(EXPECTED_NOTIFICATION_TIME).toInstant())
                .content(CONTENT)
                .note(NOTE)
                .fileIds(FILE_IDS)
                .issuedUserId(ISSUED_USER_ID)
                .build();
        Event event = new Event(command);
        assertEquals(TITLE, event.getTitle());
        assertEquals(CONTENT, event.getContent());
        assertEquals(ContentType.HTML, event.getContentType());
        assertEquals(DESCRIPTION, event.getDescription());
        Assertions.assertEquals(EventStatus.PENDING, event.getStatus());
        assertEquals(EventEffectType.ONLY_DISPLAY, event.getEffectType());
        assertEquals(NOTE, event.getNote());
        assertEquals(new SimpleDateFormat(EXPECTED_NOTIFICATION_PATTERN).parse(EXPECTED_NOTIFICATION_TIME).toInstant(), event.getExpectedNotificationAt());
        assertEquals(ISSUED_USER_ID, event.getIssuedUserId());

        // Test createNewEventFile
        assertTrue(event.getEventFiles().stream().allMatch(
                eventFile -> Objects.equals(eventFile.getEventId(), event.getId())
                        && FILE_IDS.contains(eventFile.getFileId())));

    }

    @Test
    @DisplayName("Function test create event with internal scope")
    public void testEventWithInternal() throws Exception {
        EventCreateCommand command = EventCreateCommand.builder()
                .description(DESCRIPTION)
                .title(TITLE)
                .effectType(EventEffectType.ONLY_DISPLAY)
                .targetType(null)
                .expectedNotificationAt(new SimpleDateFormat(EXPECTED_NOTIFICATION_PATTERN).parse(EXPECTED_NOTIFICATION_TIME).toInstant())
                .content(CONTENT)
                .note(NOTE)
                .fileIds(FILE_IDS)
                .issuedUserId(ISSUED_USER_ID)
                .build();
        Event event = new Event(command);
        assertEquals(TITLE, event.getTitle());
        assertEquals(CONTENT, event.getContent());
        assertEquals(ContentType.HTML, event.getContentType());
        assertEquals(DESCRIPTION, event.getDescription());
        Assertions.assertEquals(EventStatus.PENDING, event.getStatus());
        assertEquals(EventEffectType.ONLY_DISPLAY, event.getEffectType());
        assertEquals(NOTE, event.getNote());
        assertEquals(new SimpleDateFormat(EXPECTED_NOTIFICATION_PATTERN).parse(EXPECTED_NOTIFICATION_TIME).toInstant(), event.getExpectedNotificationAt());
        assertEquals(ISSUED_USER_ID, event.getIssuedUserId());

        // Test createNewEventFile
        assertTrue(event.getEventFiles().stream().allMatch(
                eventFile -> Objects.equals(eventFile.getEventId(), event.getId())
                        && FILE_IDS.contains(eventFile.getFileId())));
    }

    @Test
    @DisplayName("Function test create issued event html")
    public void testIssuedEventHtml() {
        IssueEventCmd command = IssueEventCmd.builder()
                .title(TITLE)
                .content(CONTENT)
                .targets(TARGET_IDS)
                .build();
        Event event = new Event(command);

        Assertions.assertEquals(EventStatus.IN_PROGRESS, event.getStatus());
        assertEquals(EventSource.SYSTEM, event.getEventSource());
        assertEquals(TITLE, event.getTitle());
        assertEquals(CONTENT, event.getContent());
        assertEquals(ContentType.HTML, event.getContentType());

        // compare
        assertTrue(event.getEventTargets().stream().allMatch(eventTarget ->
                Objects.equals(eventTarget.getTargetType(), TargetType.USER)
                        && TARGET_IDS.contains(eventTarget.getTarget())
        ));

        assertTrue(event.getNotifications().stream().allMatch(notification ->
                TARGET_IDS.contains(notification.getUserId())
        ));
    }

    @Test
    @DisplayName("Function test create issued event json")
    public void testIssuedEventJson() {
        IssueEventCmd command = IssueEventCmd.builder()
                .title(TITLE)
                .content(BLANK_STRING)
                .contentType(ContentType.JSON)
                .targets(TARGET_IDS)
                .build();
        Event event = new Event(command);

        Assertions.assertEquals(EventStatus.IN_PROGRESS, event.getStatus());
        assertEquals(EventSource.SYSTEM, event.getEventSource());
        assertEquals(TITLE, event.getTitle());
        assertEquals(ContentType.JSON, event.getContentType());

        // compare
        assertTrue(event.getEventTargets().stream().allMatch(eventTarget ->
                Objects.equals(eventTarget.getTargetType(), TargetType.USER)
                        && TARGET_IDS.contains(eventTarget.getTarget())
        ));

        assertTrue(event.getNotifications().stream().allMatch(notification ->
                TARGET_IDS.contains(notification.getUserId())
        ));
    }

    @Test
    @DisplayName("Function test update event")
    public void testUpdateEvent() throws Exception {
        EventCreateCommand createCommand = EventCreateCommand.builder()
                .description(DESCRIPTION)
                .title(TITLE)
                .effectType(EventEffectType.ONLY_DISPLAY)
                .targetType(null)
                .expectedNotificationAt(new SimpleDateFormat(EXPECTED_NOTIFICATION_PATTERN).parse(EXPECTED_NOTIFICATION_TIME).toInstant())
                .content(CONTENT)
                .note(NOTE)
                .fileIds(FILE_IDS)
                .issuedUserId(ISSUED_USER_ID)
                .build();

        EventUpdateCommand command = EventUpdateCommand.builder()
                .description(DESCRIPTION_UPDATE)
                .title(TITLE_UPDATE)
                .expectedNotificationAt(new SimpleDateFormat(EXPECTED_NOTIFICATION_PATTERN).parse(EXPECTED_NOTIFICATION_TIME).toInstant())
                .content(CONTENT_UPDATE)
                .note(NOTE_UPDATE)
                .fileIds(FILE_IDS_UPDATE)
                .build();

        Event event = new Event(createCommand);
        event.update(command);

        assertEquals(DESCRIPTION_UPDATE, event.getDescription());
        assertEquals(TITLE_UPDATE, event.getTitle());
        assertEquals(CONTENT_UPDATE, event.getContent());
        assertEquals(NOTE_UPDATE, event.getNote());
        assertEquals(new SimpleDateFormat(EXPECTED_NOTIFICATION_PATTERN).parse(EXPECTED_NOTIFICATION_TIME).toInstant(), event.getExpectedNotificationAt());

        // test file
        assertTrue(event.getEventFiles().stream().allMatch(
                eventFile -> (Objects.equals(eventFile.getDeleted(), Boolean.TRUE) && FILE_IDS.contains(eventFile.getFileId()))
                        || (Objects.equals(eventFile.getDeleted(), Boolean.FALSE) && FILE_IDS_UPDATE.contains(eventFile.getFileId()))
        ));
    }

    @Test
    @DisplayName("Function test update event with customer scope")
    public void testUpdateEventWithCustomerScope() throws Exception {
        EventCreateCommand createCommand = EventCreateCommand.builder()
                .description(DESCRIPTION)
                .title(TITLE)
                .effectType(EventEffectType.ONLY_DISPLAY)
                .targetType(null)
                .expectedNotificationAt(new SimpleDateFormat(EXPECTED_NOTIFICATION_PATTERN).parse(EXPECTED_NOTIFICATION_TIME).toInstant())
                .content(CONTENT)
                .note(NOTE)
                .fileIds(FILE_IDS)
                .issuedUserId(ISSUED_USER_ID)
                .build();

        EventUpdateCommand command = EventUpdateCommand.builder()
                .description(DESCRIPTION_UPDATE)
                .title(TITLE_UPDATE)
                .expectedNotificationAt(new SimpleDateFormat(EXPECTED_NOTIFICATION_PATTERN).parse(EXPECTED_NOTIFICATION_TIME).toInstant())
                .content(CONTENT_UPDATE)
                .note(NOTE_UPDATE)
                .fileIds(FILE_IDS_UPDATE)
                .build();

        Event event = new Event(createCommand);
        event.update(command);

        assertEquals(DESCRIPTION_UPDATE, event.getDescription());
        assertEquals(TITLE_UPDATE, event.getTitle());
        assertEquals(CONTENT_UPDATE, event.getContent());
        assertEquals(NOTE_UPDATE, event.getNote());
        assertEquals(new SimpleDateFormat(EXPECTED_NOTIFICATION_PATTERN).parse(EXPECTED_NOTIFICATION_TIME).toInstant(), event.getExpectedNotificationAt());

        // test file
        assertTrue(event.getEventFiles().stream().allMatch(
                eventFile -> (Objects.equals(eventFile.getDeleted(), Boolean.TRUE) && FILE_IDS.contains(eventFile.getFileId()))
                        || (Objects.equals(eventFile.getDeleted(), Boolean.FALSE) && FILE_IDS_UPDATE.contains(eventFile.getFileId()))
        ));
    }

    @Test
    @DisplayName("Function test delete event")
    void delete() {
        Event event = Event.builder()
                .id(evtId)
                .eventFiles(List.copyOf(eventFiles))
                .eventTargets(List.copyOf(eventTargets))
                .deleted(false)
                .build();

        event.delete();

        assertTrue(event.getDeleted());
        assertFalse(event.getEventFiles().stream().anyMatch(it -> Objects.equals(it.getDeleted(), Boolean.FALSE)));
        assertFalse(event.getEventTargets().stream().anyMatch(it -> Objects.equals(it.getDeleted(), Boolean.FALSE)));
    }


    @Test
    @DisplayName("Function test send event")
    void sent() {
        Event event = Event.builder()
                .id(evtId)
                .eventFiles(List.copyOf(eventFiles))
                .eventTargets(List.copyOf(eventTargets))
                .deleted(false)
                .build();

        event.sent();
        Assertions.assertEquals(EventStatus.DONE, event.getStatus());

    }

    @Test
    @DisplayName("Function test send notification event")
    void sentNotification() {
        Event event = Event.builder()
                .id(evtId)
                .eventFiles(List.copyOf(eventFiles))
                .eventTargets(List.copyOf(eventTargets))
                .notifications(List.copyOf(notifications))
                .deleted(false)
                .build();

        event.sent();
        Assertions.assertEquals(EventStatus.DONE, event.getStatus());
        assertTrue(event.getNotifications().stream()
                .allMatch(it -> Objects.equals(it.getIsSend(), Boolean.TRUE)
                        && Objects.nonNull(it.getSendAt())));
    }

    @Test
    @DisplayName("Function test pre send notification or noti_email event")
    void preSentNotificationEndEmail() {
        Event event = Event.builder()
                .id(evtId)
                .types(Set.of(EventType.NOTIFICATION))
                .eventFiles(List.copyOf(eventFiles))
                .eventTargets(List.copyOf(eventTargets))
                .targets(List.copyOf(targets))
                .deleted(false)
                .build();
        event.preSent(currentUserId);

        Assertions.assertEquals(EventStatus.IN_PROGRESS, event.getStatus());
        assertEquals(event.getSenderUserId(), currentUserId);

        assertFalse(CollectionUtils.isEmpty(event.getNotifications()));
        assertEquals(event.getNotifications().size(), targets.size());
        List<String> targetIds = event.getTargets().stream().map(Target::getUserId).collect(Collectors.toList());

        assertTrue(event.getNotifications().stream().allMatch(it -> Objects.equals(it.getEventId(), event.getId())
                && targetIds.contains(it.getUserId())));

    }

    @Test
    @DisplayName("Function test pre send email event")
    void preSentEmail() {
        Event event = Event.builder()
                .id(evtId)
                .eventFiles(List.copyOf(eventFiles))
                .eventTargets(List.copyOf(eventTargets))
                .targets(List.copyOf(targets))
                .deleted(false)
                .build();
        event.preSent(currentUserId);

        Assertions.assertEquals(EventStatus.IN_PROGRESS, event.getStatus());
        assertEquals(event.getSenderUserId(), currentUserId);

    }

    @Test
    @DisplayName("Function test send failed event")
    void failed() {
        Event event = Event.builder().id(evtId).deleted(false).build();
        event.failed(REASON);

        Assertions.assertEquals(EventStatus.FAILED, event.getStatus());
        assertEquals(REASON, event.getFailureCauses());
    }

}
