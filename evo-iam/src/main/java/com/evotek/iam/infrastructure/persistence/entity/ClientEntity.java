package com.evotek.iam.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.ClientStatus;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "client", indexes = {
        @Index(name = "client_name_idx", columnList = "name"),
        @Index(name = "client_deleted_idx", columnList = "deleted")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class ClientEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "name", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, nullable = false)
    private String name;

    @Column(name = "secret", length = ValidateConstraint.LENGTH.VALUE_MAX_LENGTH, nullable = false)
    private String secret;

    @Column(name = "status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private ClientStatus status;

    @Column(name = "role_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String roleId;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        ClientEntity that = (ClientEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
