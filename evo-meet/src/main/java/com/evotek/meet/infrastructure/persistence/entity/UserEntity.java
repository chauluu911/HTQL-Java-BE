package com.evotek.meet.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import lombok.*;

import javax.persistence.*;
import java.time.Instant;
import java.util.Objects;

@Entity
@Table(name = "users")
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class UserEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "last_modified_scheduler_at")
    private Instant lastModifiedSchedulerAt;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Version
    @Column(name = "version")
    private Long version;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        UserEntity that = (UserEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
