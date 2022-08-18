package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.enums.Gender;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.time.LocalDate;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class UserUpdateRequest extends Request {

    @NotBlank(message = "FULL_NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "FULL_NAME_LENGTH")
    private String fullName;

    @Size(min = ValidateConstraint.LENGTH.PASSWORD_MIN_LENGTH, max = ValidateConstraint.LENGTH.PASSWORD_MAX_LENGTH,
            message = "PASSWORD_LENGTH")
    @Pattern(regexp = "^(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9])(?=.*[\\^$*.\\[\\]{}\\(\\)?\\-“!@#%&/,><\\’:;|_~`])\\S{8,99}$",
            message = "FORMAT_PASSWORD")
    private String password;

    @NotBlank(message = "EMAIL_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.EMAIL_MAX_LENGTH, message = "EMAIL_MAX_LENGTH")
    @Pattern(regexp = ValidateConstraint.FORMAT.EMAIL_PATTERN, message = "EMAIL_WRONG_FORMAT")
    private String email;

    @NotBlank(message = "PHONE_NUMBER_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.PHONE_MAX_LENGTH, message = "PHONE_NUMBER_LENGTH")
    @Pattern(regexp = ValidateConstraint.FORMAT.PHONE_NUMBER_PATTERN, message = "PHONE_NUMBER_FORMAT")
    private String phoneNumber;

    private LocalDate dayOfBirth;

    private Gender gender;

    private List<String> roleIds;

    @Pattern(regexp = "^[A-Za-z0-9]+$", message = "FORMAT_EMPLOYEE_CODE")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "EMPLOYEE_CODE_LENGTH")
    private String employeeCode;

    @Size(max = ValidateConstraint.LENGTH.TITLE_MAX_LENGTH, message = "TITLE_LENGTH")
    private String title;

    @Size(max = ValidateConstraint.LENGTH.DESC_MAX_LENGTH, message = "DESC_LENGTH")
    private String description;

    private UserStatus status;

    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "DEPARTMENT_NAME_LENGTH")
    private String departmentName;

    @Size(min = ValidateConstraint.LENGTH.ID_MIN_LENGTH,
            max = ValidateConstraint.LENGTH.ID_MAX_LENGTH, message = "ID_LENGTH")
    private String avatarFileId;
    @Size(max = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String jobTitleId;

    @Valid
    private EmploymentCreateOrUpdateRequest employment;
}
