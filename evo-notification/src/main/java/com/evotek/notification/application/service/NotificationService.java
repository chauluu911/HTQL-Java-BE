package com.evotek.notification.application.service;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.webapp.support.DomainService;
import com.evotek.notification.application.dto.request.NotificationDeleteRequest;
import com.evotek.notification.application.dto.request.NotificationMarkReadRequest;
import com.evotek.notification.application.dto.request.NotificationMarkUnreadRequest;
import com.evotek.notification.application.dto.request.NotificationSearchRequest;
import com.evotek.notification.domain.Notification;

public interface NotificationService extends DomainService<Notification, String> {

    Notification ensureExisted(String uuid);

    Notification getNotificationByEventId(String eventId);

    Notification getNotificationById(String id);

    PageDTO<Notification> findAllByUserLogin(NotificationSearchRequest params);

    Boolean markAllRead(NotificationMarkReadRequest notificationMarkReadRequest);

    Boolean markAllUnreadByIds(NotificationMarkUnreadRequest notificationMarkUnreadRequest);

    Boolean markRead(String id);

    Boolean deleteAll(NotificationDeleteRequest notificationMarkReadRequest);

    Boolean delete(String id);

    Long countUnreadNotification();

    Boolean markReadAll();

    String getUrlJoinTelegramBot();
}
