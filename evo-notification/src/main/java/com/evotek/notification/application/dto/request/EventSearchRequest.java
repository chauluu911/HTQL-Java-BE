package com.evotek.notification.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import com.evotek.notification.infrastructure.support.enums.EventSendTo;
import com.evotek.notification.infrastructure.support.enums.EventStatus;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.Instant;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class EventSearchRequest extends PagingRequest {

    private String keyword;

    private EventStatus status;

    private Instant expectedNotificationStartAt;

    private Instant expectedNotificationEndAt;

    private Instant notificationStartAt;

    private Instant notificationEndAt;

    private String issuedUserId;

    private EventSendTo sendTo;

}
