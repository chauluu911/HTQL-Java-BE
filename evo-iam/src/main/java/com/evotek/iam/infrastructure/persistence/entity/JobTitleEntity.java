package com.evotek.iam.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.JobTitleStatus;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.hibernate.Hibernate;

import javax.persistence.*;
import javax.validation.constraints.Size;
import java.util.Objects;

@Entity
@Table(name = "job_title", indexes = {
        @Index(name = "job_title_code_idx", columnList = "code"),
        @Index(name = "job_title_deleted_idx", columnList = "deleted")
})
@Data
@RequiredArgsConstructor
@AllArgsConstructor
public class JobTitleEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "code", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, nullable = false)
    private String code;

    @Column(name = "name", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, nullable = false)
    private String name;

    @Column(name = "status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private JobTitleStatus status;

    @Column(name = "description")
    private String description;

    @Column(name = "manager")
    private Boolean isManager;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    public void delete(){
        this.deleted = false;
    }
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        JobTitleEntity that = (JobTitleEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
