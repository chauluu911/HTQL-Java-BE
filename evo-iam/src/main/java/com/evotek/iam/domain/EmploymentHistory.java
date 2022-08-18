package com.evotek.iam.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import com.evotek.iam.domain.command.EmploymentCreateOrUpdateCmd;
import com.evotek.iam.infrastructure.support.enums.EmploymentStatus;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;
import java.util.Objects;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class EmploymentHistory extends AuditableDomain {
    private String id;

    private String departmentId;

    private String userId;

    private Boolean deleted;

    private EmploymentStatus status;

    private LocalDate startDate;

    private LocalDate endDate;

    private String fullName;

    private String departmentName;

    private String jobTitleId;

    private String jobTitleCode;

    private String jobTitleName;

    private com.evotek.iam.domain.JobTitle jobTitle;

    public EmploymentHistory(String userId, EmploymentCreateOrUpdateCmd cmd) {
        this.id = IdUtils.nextId();
        this.userId = userId;
        this.departmentId = cmd.getDepartmentId();
        this.status = EmploymentStatus.ACTIVE;
        this.startDate = cmd.getStartDate();
        this.endDate = cmd.getEndDate();
        this.jobTitleId = cmd.getJobTitleId();
        this.deleted = false;
    }

    public void enrichFullName(String fullName) {
        this.setFullName(fullName);
    }

    public void enrichDepartmentName(String departmentName) {
        this.setDepartmentName(departmentName);
    }

    public void enrichJobTitleCode(String jobTitleCode) {
        this.jobTitleCode = jobTitleCode;
    }

    public void enrichJobTitleName(String jobTitleName) {
        this.jobTitleName = jobTitleName;
    }

    public void enrichJobTitle(com.evotek.iam.domain.JobTitle jobTitle){
        this.jobTitle = jobTitle;
    }

    public void update(EmploymentCreateOrUpdateCmd cmd) {
        this.startDate = cmd.getStartDate();
        this.endDate = cmd.getEndDate();
        this.jobTitleId = cmd.getJobTitleId();
        this.status = EmploymentStatus.ACTIVE;
        this.deleted = false;
    }

    public void leaveDepartment() {
        if (Objects.equals(this.status, EmploymentStatus.ACTIVE)) {
            this.status = EmploymentStatus.INACTIVE;
            this.endDate = LocalDate.now();
            this.deleted = false;
        }
    }

    public void delete() {
        this.deleted = true;
    }

}
