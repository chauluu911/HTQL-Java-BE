package com.evotek.notification.domain;

import com.evotek.common.util.IdUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class NotificationTest {

    private final String userId = IdUtils.nextId();
    private final String eventId = IdUtils.nextId();
    private final String notificationId = IdUtils.nextId();


    @BeforeEach
    void setUp() {
    }

    Notification generateNotification() {
        return Notification.builder()
                .id(notificationId)
                .userId(IdUtils.nextId())
                .eventId(IdUtils.nextId())
                .isSend(false)
                .isRead(false)
                .deleted(false)
                .build();
    }

    @Test
    @DisplayName("Function test create notification")
    void create() {
        Notification notification = new Notification(userId, eventId);
        assertFalse(notification.getIsSend());
        assertFalse(notification.getIsRead());
        assertFalse(notification.getDeleted());
        assertEquals(userId, notification.getUserId());
        assertEquals(eventId, notification.getEventId());
    }

    @Test
    @DisplayName("Function test read notification")
    void read() {
        Notification notification = generateNotification();
        notification.read();

        assertTrue(notification.getIsRead());
        assertNotNull(notification.getReadAt());
    }

    @Test
    @DisplayName("Function test read notification")
    void unread() {
        Notification notification = generateNotification();
        notification.unread();

        assertFalse(notification.getIsRead());
        assertNull(notification.getReadAt());
    }

    @Test
    @DisplayName("Function test send notification")
    void sent() {
        Notification notification = generateNotification();
        notification.sent();

        assertTrue(notification.getIsSend());
        assertNotNull(notification.getSendAt());
    }

    @Test
    @DisplayName("Function test delete notification")
    void deleted() {
        Notification notification = generateNotification();
        notification.deleted();
        assertTrue(notification.getDeleted());
    }

}