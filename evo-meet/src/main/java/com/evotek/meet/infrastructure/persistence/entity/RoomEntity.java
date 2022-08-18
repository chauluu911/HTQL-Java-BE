package com.evotek.meet.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.meet.infrastructure.support.enums.RoomStatus;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "room", indexes = {
        @Index(name = "room_code_idx", columnList = "code"),
        @Index(name = "room_deleted_idx", columnList = "deleted")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class RoomEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "code", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, nullable = false)
    private String code;

    @Column(name = "name", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, nullable = false)
    private String name;

    @Column(name = "location", length = ValidateConstraint.LENGTH.ADDRESS_MAX_LENGTH, nullable = false)
    private String location;

    @Column(name = "status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private RoomStatus status = RoomStatus.ACTIVE;

    @Version
    @Column(name = "version")
    private Long version;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        RoomEntity that = (RoomEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
