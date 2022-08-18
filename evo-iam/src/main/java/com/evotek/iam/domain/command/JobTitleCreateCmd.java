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
public class JobTitleCreateCmd {
    private String name;
    private String code;
    private String description;
    private Boolean isManager;
}
