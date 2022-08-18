package com.evotek.iam.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "user_group_member", indexes = {
        @Index(name = "user_group_member_group_id_idx", columnList = "group_id"),
        @Index(name = "user_group_member_user_id_idx", columnList = "user_id"),
        @Index(name = "user_group_member_deleted_idx", columnList = "deleted"),

})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class UserGroupMemberEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;
    @Column(name = "user_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String userId;
    @Column(name = "group_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String groupId;
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