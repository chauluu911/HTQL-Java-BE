package com.evotek.notification.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.enums.ContentType;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.notification.infrastructure.support.enums.EventEffectType;
import com.evotek.notification.infrastructure.support.enums.EventSendTo;
import com.evotek.notification.infrastructure.support.enums.EventSource;
import com.evotek.notification.infrastructure.support.enums.EventStatus;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.time.Instant;
import java.util.Objects;

@Entity
@Table(name = "event", indexes = {
        @Index(name = "event_issued_user_id_idx", columnList = "issued_user_id"),
        @Index(name = "event_deleted_idx", columnList = "deleted"),
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class EventEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "description", length = ValidateConstraint.LENGTH.DESC_MAX_LENGTH)
    private String description;

    @Column(name = "title", length = ValidateConstraint.LENGTH.TITLE_MAX_LENGTH, nullable = false)
    private String title;

    @Column(name = "effect_type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH)
    @Enumerated(EnumType.STRING)
    private EventEffectType effectType;

    @Column(name = "status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private EventStatus status;

    @Column(name = "event_source", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH)
    @Enumerated(EnumType.STRING)
    private EventSource eventSource;

    @Column(name = "deleted")
    private Boolean deleted;

    @Column(name = "notification_at")
    private Instant notificationAt;

    @Column(name = "content", nullable = false, columnDefinition = "text")
    private String content;

    @Column(name = "content_type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private ContentType contentType;

    @Column(name = "note", length = ValidateConstraint.LENGTH.NOTE_MAX_LENGTH)
    private String note;

    @Column(name = "issued_user_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String issuedUserId;

    @Column(name = "sender_user_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String senderUserId;

    @Column(name = "expected_notification_at")
    private Instant expectedNotificationAt;

    @Column(name = "failure_causes", length = ValidateConstraint.LENGTH.NOTE_MAX_LENGTH)
    private String failureCauses;

    @Column(name = "types")
    private String types;

    @Column(name = "send_to", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH)
    @Enumerated(EnumType.STRING)
    private EventSendTo sendTo;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        EventEntity that = (EventEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
