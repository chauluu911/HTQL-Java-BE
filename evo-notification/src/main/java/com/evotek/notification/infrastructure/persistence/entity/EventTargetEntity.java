package com.evotek.notification.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.enums.TargetType;
import com.evotek.common.validator.ValidateConstraint;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "event_target", indexes = {
        @Index(name = "event_target_event_id_idx", columnList = "event_id"),
        @Index(name = "event_target_target_type_idx", columnList = "target_type"),
        @Index(name = "event_target_target_idx", columnList = "target")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class EventTargetEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "target_type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private TargetType targetType;

    @Column(name = "event_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String eventId;

    @Column(name = "target", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String target;

    @Column(name = "deleted")
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        EventTargetEntity that = (EventTargetEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
