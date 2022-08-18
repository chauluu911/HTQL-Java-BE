package com.evotek.common.ldapcore.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
public class UserLdapCreateOrUpdateRequest extends Request {

    private String fullName;
    private String username;

    @Size(min = ValidateConstraint.LENGTH.PASSWORD_MIN_LENGTH, max = ValidateConstraint.LENGTH.PASSWORD_MAX_LENGTH,
            message = "PASSWORD_LENGTH")
    @NotBlank(message = "PASSWORD_REQUIRED")
    @Pattern(regexp = "^(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9])(?=.*[\\^$*.\\[\\]{}\\(\\)?\\-“!@#%&/,><\\’:;|_~`])\\S{8,99}$",
            message = "FORMAT_PASSWORD")
    private String password;
}
