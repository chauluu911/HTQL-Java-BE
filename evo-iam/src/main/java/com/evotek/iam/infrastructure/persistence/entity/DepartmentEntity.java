package com.evotek.iam.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.DepartmentStatus;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "department", indexes = {
        @Index(name = "department_code_idx", columnList = "code"),
        @Index(name = "department_deleted_idx", columnList = "deleted")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class DepartmentEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "name", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, nullable = false)
    private String name;

    @Column(name = "code", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, nullable = false, unique = true)
    private String code;

    @Column(name = "deleted")
    private Boolean deleted;

    @Column(name = "description", length = ValidateConstraint.LENGTH.DESC_MAX_LENGTH)
    private String description;

    @Column(name = "parent_path", length = ValidateConstraint.LENGTH.NOTE_MAX_LENGTH)
    private String parentPath;

    @Column(name = "parent_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String parentId;

    @Column(name = "status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private DepartmentStatus status = DepartmentStatus.ACTIVE;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        DepartmentEntity that = (DepartmentEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
