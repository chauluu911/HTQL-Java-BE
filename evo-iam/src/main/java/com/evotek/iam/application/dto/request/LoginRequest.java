package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
public class LoginRequest extends Request {

    @NotBlank(message = "USERNAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "USERNAME_LENGTH")
    private String username;

    @NotBlank(message = "PASSWORD_REQUIRED")
    @Size(min = ValidateConstraint.LENGTH.PASSWORD_MIN_LENGTH, max = ValidateConstraint.LENGTH.PASSWORD_MAX_LENGTH,
            message = "PASSWORD_LENGTH")
    private String password;

    private boolean rememberMe = false;
}
