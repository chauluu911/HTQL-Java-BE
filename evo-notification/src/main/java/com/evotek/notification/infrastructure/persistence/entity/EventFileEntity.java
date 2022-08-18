package com.evotek.notification.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.notification.infrastructure.support.enums.EventFileType;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "event_file", indexes = {
        @Index(name = "event_file_event_id_idx", columnList = "event_id"),
        @Index(name = "event_file_file_id_idx", columnList = "file_id")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class EventFileEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "file_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String fileId;

    @Column(name = "event_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String eventId;

    @Column(name = "type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH)
    @Enumerated(EnumType.STRING)
    private EventFileType type;

    @Column(name = "deleted")
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        EventFileEntity that = (EventFileEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
