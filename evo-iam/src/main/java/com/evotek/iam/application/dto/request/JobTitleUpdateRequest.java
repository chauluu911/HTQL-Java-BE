package com.evotek.iam.application.dto.request;

import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.JobTitleStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class JobTitleUpdateRequest {

    @NotBlank(message = "NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "NAME_LENGTH")
    private String name;

    private JobTitleStatus status;

    @Size(max = ValidateConstraint.LENGTH.DESC_MAX_LENGTH, message = "DESCRIPTION_LENGTH")
    private String description;

    private Boolean isManager;
}
