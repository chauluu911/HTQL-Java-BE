package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
public class UserBaseInformationRequest extends Request {

    @Pattern(regexp = "^[A-Za-z0-9._]+$", message = "USERNAME_FORMAT")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "USERNAME_LENGTH")
    private String username;

    @Size(min = ValidateConstraint.LENGTH.PASSWORD_MIN_LENGTH, max = ValidateConstraint.LENGTH.PASSWORD_MAX_LENGTH,
            message = "PASSWORD_LENGTH")
    @Pattern(regexp = "^(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9])(?=.*[\\^$*.\\[\\]{}\\(\\)?\\-“!@#%&/,><\\’:;|_~`])\\S{8,99}$",
            message = "FORMAT_PASSWORD")
    private String password;

    private String repeatPassword;

    @NotBlank(message = "EMAIL_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.EMAIL_MAX_LENGTH, message = "EMAIL_MAX_LENGTH")
    @Pattern(regexp = ValidateConstraint.FORMAT.EMAIL_PATTERN, message = "EMAIL_WRONG_FORMAT")
    private String email;

    @NotBlank(message = "PHONE_NUMBER_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.PHONE_MAX_LENGTH, message = "PHONE_NUMBER_LENGTH")
    @Pattern(regexp = ValidateConstraint.FORMAT.PHONE_NUMBER_PATTERN, message = "PHONE_NUMBER_FORMAT")
    private String phoneNumber;

    @Pattern(regexp = "^[A-Za-z0-9]+$", message = "FORMAT_EMPLOYEE_CODE")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "EMPLOYEE_CODE_LENGTH")
    private String employeeCode;
}
