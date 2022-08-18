package com.evotek.notification.domain.command;

import com.evotek.common.enums.EventType;
import com.evotek.common.enums.TargetType;
import com.evotek.notification.infrastructure.support.enums.EventEffectType;
import com.evotek.notification.infrastructure.support.enums.EventSendTo;
import com.evotek.notification.infrastructure.support.enums.EventSource;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;
import java.util.List;
import java.util.Set;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class EventCreateCommand {

    private String description;

    private String title;

    private Set<EventType> types;

    private EventSource eventSource;

    private EventEffectType effectType;

    private TargetType targetType;

    private Instant expectedNotificationAt;

    private String content;

    private String note;

    private List<String> fileIds;

    private String issuedUserId;

    private List<String> userIds;

    private List<String> departmentIds;

    private EventSendTo sendTo;

}
