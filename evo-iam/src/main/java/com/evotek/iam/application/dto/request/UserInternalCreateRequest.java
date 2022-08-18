package com.evotek.iam.application.dto.request;

import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.AuthenticationType;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
public class UserInternalCreateRequest extends UserCreateRequest {

    @Pattern(regexp = "^[A-Za-z0-9]+$", message = "FORMAT_EMPLOYEE_CODE")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "EMPLOYEE_CODE_LENGTH")
    private String employeeCode;

    @Size(max = ValidateConstraint.LENGTH.TITLE_MAX_LENGTH, message = "TITLE_LENGTH")
    private String title;

    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "DEPARTMENT_NAME_LENGTH")
    private String departmentName;

    @Size(min = ValidateConstraint.LENGTH.PASSWORD_MIN_LENGTH, max = ValidateConstraint.LENGTH.PASSWORD_MAX_LENGTH,
            message = "PASSWORD_LENGTH")
    @NotBlank(message = "PASSWORD_REQUIRED")
    @Pattern(regexp = "^(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9])(?=.*[\\^$*.\\[\\]{}\\(\\)?\\-“!@#%&/,><\\’:;|_~`])\\S{8,99}$",
            message = "FORMAT_PASSWORD")
    private String password;

    @Size(min = ValidateConstraint.LENGTH.PASSWORD_MIN_LENGTH, max = ValidateConstraint.LENGTH.PASSWORD_MAX_LENGTH,
            message = "PASSWORD_LENGTH")
    @NotBlank(message = "REPEAT_PASSWORD_REQUIRED")
    @Pattern(regexp = "^(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9])(?=.*[\\^$*.\\[\\]{}\\(\\)?\\-“!@#%&/,><\\’:;|_~`])\\S{8,99}$",
            message = "FORMAT_PASSWORD")
    private String repeatPassword;

    private AuthenticationType authenticationType;

}
