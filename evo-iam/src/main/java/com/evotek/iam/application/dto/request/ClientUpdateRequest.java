package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.ClientStatus;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
public class ClientUpdateRequest extends Request {
    @NotBlank(message = "NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "NAME_LENGTH")
    private String name;

    @NotNull(message = "CLIENT_STATUS_REQUIRED")
    private ClientStatus status;


    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "ROLE_ID_LENGTH")
    private String roleId;
}
