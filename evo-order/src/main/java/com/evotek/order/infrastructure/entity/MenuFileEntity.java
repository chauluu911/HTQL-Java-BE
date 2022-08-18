package com.evotek.order.infrastructure.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.Hibernate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Objects;

@Entity
@Table(name = "menu_file")
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class MenuFileEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "menu_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String menuId;

    @Column(name = "file_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String fileId;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        MenuFileEntity that = (MenuFileEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
