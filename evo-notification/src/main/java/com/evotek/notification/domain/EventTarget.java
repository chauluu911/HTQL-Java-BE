package com.evotek.notification.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.enums.TargetType;
import com.evotek.common.util.IdUtils;
import lombok.*;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class EventTarget extends AuditableDomain {

    private String id;
    private TargetType targetType;
    private String eventId;
    private String target;
    private Boolean deleted;

    public EventTarget(String eventId, String target, TargetType targetType) {
        this.id = IdUtils.nextId();
        this.eventId = eventId;
        this.target = target;
        this.targetType = targetType;
        this.deleted = false;
    }

    public void delete() {
        this.deleted = true;
    }

    public void unDeleted() {
        this.deleted = false;
    }
}
