package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
public class DepartmentUpdateRequest extends Request {

    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH,
            message = "DEPARTMENT_NAME_LENGTH")
    private String name;

    @Size(max = ValidateConstraint.LENGTH.DESC_MAX_LENGTH, message = "DEPARTMENT_DESCRIPTION_LENGTH")
    private String description;

    private String parentId;
}
