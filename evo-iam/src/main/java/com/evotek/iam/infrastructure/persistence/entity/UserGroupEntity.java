package com.evotek.iam.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "user_group", indexes = {
        @Index(name = "user_group_name_idx", columnList = "name"),
        @Index(name = "user_group_code_idx", columnList = "code")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class UserGroupEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;
    @Column(name = "name", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH)
    private String name;
    @Column(name = "code", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH)
    private String code;
    @Column(name = "description", length = ValidateConstraint.LENGTH.DESC_MAX_LENGTH)
    private String description;
    @Column(name = "total_member")
    private Integer totalMember;
    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        ClientEntity that = (ClientEntity) o;
        return id != null && Objects.equals(id, that.getId());
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }

}
