package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.JobTitleStatus;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
public class JobTitleCreateRequest extends Request {

    @NotBlank(message = "CODE_REQUIRED")
    @Pattern(regexp = "^[A-Za-z0-9_]+$", message = "FORMAT_JOB_TITLE_CODE")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "CODE_LENGTH")
    private String code;

    @NotBlank(message = "NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "NAME_LENGTH")
    private String name;

    @Size(max = ValidateConstraint.LENGTH.DESC_MAX_LENGTH, message = "DESCRIPTION_LENGTH")
    private String description;

    private Boolean isManager;

}
