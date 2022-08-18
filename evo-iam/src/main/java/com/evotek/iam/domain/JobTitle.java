package com.evotek.iam.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import com.evotek.iam.domain.command.JobTitleCreateCmd;
import com.evotek.iam.domain.command.JobTitleUpdateCmd;
import com.evotek.iam.infrastructure.support.enums.JobTitleStatus;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.Instant;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class JobTitle extends AuditableDomain {
    private String id;
    private String code;
    private String name;
    private JobTitleStatus status;
    private String description;
    private Boolean isManager;
    private Boolean deleted;

    public JobTitle(JobTitleCreateCmd cmd) {
        this.id = IdUtils.nextId();
        this.code = cmd.getCode();
        this.name = cmd.getName();
        this.status = JobTitleStatus.ACTIVE;
        this.description = cmd.getDescription();
        this.isManager = cmd.getIsManager();
        this.deleted = false;
    }

    public void update(JobTitleUpdateCmd cmd) {
        this.name = cmd.getName() != null ? cmd.getName() : this.name;
        this.status = cmd.getStatus() != null ? cmd.getStatus() : this.status;
        this.description = cmd.getDescription() != null ? cmd.getDescription() : this.getDescription();
        this.isManager = cmd.getIsManager();
    }

    public void delete(){
        this.deleted = true;
    }

    public void active(){
        this.status = JobTitleStatus.ACTIVE;
    }

    public void inactive(){
        this.status = JobTitleStatus.INACTIVE;
    }

    public void manageManager(boolean isManager){
        this.isManager = isManager;
    }

}
