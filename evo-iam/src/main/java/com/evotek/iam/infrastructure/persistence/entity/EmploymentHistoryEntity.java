package com.evotek.iam.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.EmploymentStatus;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.time.LocalDate;
import java.util.Objects;

@Entity
@Table(name = "employment_history", indexes = {
        @Index(name = "employment_history_department_id_idx", columnList = "department_id"),
        @Index(name = "employment_history_user_id_idx", columnList = "user_id")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class EmploymentHistoryEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "department_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String departmentId;

    @Column(name = "user_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String userId;

    @Column(name = "status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH)
    @Enumerated(EnumType.STRING)
    private EmploymentStatus status;

    @Column(name = "start_date")
    private LocalDate startDate;

    @Column(name = "end_date")
    private LocalDate endDate;

    @Column(name = "job_title_id")
    private String jobTitleId;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        EmploymentHistoryEntity that = (EmploymentHistoryEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
