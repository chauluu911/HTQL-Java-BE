package com.evotek.notification.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.time.Instant;
import java.util.Objects;

@Entity
@Table(name = "notification", indexes = {
        @Index(name = "notification_user_id_idx", columnList = "user_id"),
        @Index(name = "notification_event_id_idx", columnList = "event_id"),
        @Index(name = "notification_deleted_idx", columnList = "deleted")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class NotificationEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "user_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String userId;

    @Column(name = "event_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String eventId;

    @Column(name = "is_read")
    private Boolean isRead;

    @Column(name = "read_at")
    private Instant readAt;

    @Column(name = "is_send")
    private Boolean isSend;

    @Column(name = "send_at")
    private Instant sendAt;

    @Column(name = "deleted")
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        NotificationEntity that = (NotificationEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
