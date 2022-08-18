package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class RoleUpdateRequest extends Request {

    @NotBlank(message = "ROLE_NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "ROLE_NAME_LENGTH")
    private String name;

    @Size(max = ValidateConstraint.LENGTH.DESC_MAX_LENGTH, message = "ROLE_DESCRIPTION_LENGTH")
    private String description;

    private Boolean isRoot;

    @Valid
    private List<RolePermissionCreateRequest> permissions;
}
