package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
public class DepartmentCreateRequest extends Request {

    @NotBlank(message = "DEPARTMENT_NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH,
            message = "DEPARTMENT_NAME_LENGTH")
    private String name;

    @Size(max = ValidateConstraint.LENGTH.DESC_MAX_LENGTH, message = "DEPARTMENT_DESCRIPTION_LENGTH")
    private String description;

    @Size(min = ValidateConstraint.LENGTH.ID_MIN_LENGTH,
            max = ValidateConstraint.LENGTH.ID_MAX_LENGTH, message = "ID_LENGTH")
    private String parentId;

    @NotNull(message = "DEPARTMENT_CODE_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "DEPARTMENT_CODE_LENGTH")
    @Pattern(regexp = "^[A-Za-z0-9]+$", message = "DEPARTMENT_CODE_FORMAT")
    private String code;
}
