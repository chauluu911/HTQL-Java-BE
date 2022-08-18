package com.evotek.iam.domain.command;

import com.evotek.iam.infrastructure.support.enums.JobTitleStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class JobTitleUpdateCmd {
    private String name;
    private JobTitleStatus status;
    private String description;
    private Boolean isManager;
}
