package com.evotek.notification.domain.query;

import com.evotek.common.query.PagingQuery;
import com.evotek.notification.infrastructure.support.enums.EventSendTo;
import com.evotek.notification.infrastructure.support.enums.EventSource;
import com.evotek.notification.infrastructure.support.enums.EventStatus;
import lombok.Data;
import lombok.experimental.SuperBuilder;

import java.time.Instant;

@Data
@SuperBuilder
public class EventSearchQuery extends PagingQuery {
    private EventStatus status;
    private Instant expectedNotificationStartAt;
    private Instant expectedNotificationEndAt;
    private Instant notificationStartAt;
    private Instant notificationEndAt;
    private String issuedUserId;
    private EventSource eventSource;
    private EventSendTo sendTo;
}
