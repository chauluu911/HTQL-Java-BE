package com.evotek.iam.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.enums.Scope;
import com.evotek.common.validator.ValidateConstraint;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "permission", indexes = {
        @Index(name = "permission_resource_code_idx", columnList = "resource_code"),
        @Index(name = "permission_scope_idx", columnList = "scope"),
        @Index(name = "permission_deleted_idx", columnList = "deleted")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class PermissionEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "scope", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private Scope scope;

    @Column(name = "resource_code", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, nullable = false)
    private String resourceCode;

    @Column(name = "name", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, nullable = false)
    private String name;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Column(name = "priority", nullable = false)
    private Integer priority;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        PermissionEntity that = (PermissionEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
