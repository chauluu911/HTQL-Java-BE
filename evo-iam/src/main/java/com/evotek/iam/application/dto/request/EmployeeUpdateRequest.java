package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.enums.Gender;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.time.LocalDate;

@EqualsAndHashCode(callSuper = true)
@Data
public class EmployeeUpdateRequest extends Request {
    @NotBlank(message = "FULL_NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "FULL_NAME_LENGTH")
    private String fullName;

    @NotBlank(message = "EMAIL_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.EMAIL_MAX_LENGTH, message = "EMAIL_MAX_LENGTH")
    @Pattern(regexp = ValidateConstraint.FORMAT.EMAIL_PATTERN, message = "EMAIL_WRONG_FORMAT")
    private String email;

    @NotBlank(message = "PHONE_NUMBER_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.PHONE_MAX_LENGTH, message = "PHONE_NUMBER_LENGTH")
    @Pattern(regexp = ValidateConstraint.FORMAT.PHONE_NUMBER_PATTERN, message = "PHONE_NUMBER_FORMAT")
    private String phoneNumber;

    private LocalDate dayOfBirth;
    @NotBlank(message = "GENDER_REQUIRED")
    private Gender gender;

    @Size(max = ValidateConstraint.LENGTH.TITLE_MAX_LENGTH, message = "TITLE_LENGTH")
    private String title;

    @Size(max = ValidateConstraint.LENGTH.DESC_MAX_LENGTH, message = "DESC_LENGTH")
    private String description;

    private UserStatus status;

    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "DEPARTMENT_NAME_LENGTH")
    private String departmentName;

    private String departmentId;

    @Size(min = ValidateConstraint.LENGTH.ID_MIN_LENGTH,
            max = ValidateConstraint.LENGTH.ID_MAX_LENGTH, message = "ID_LENGTH")
    private String avatarFileId;

    @Size(max = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String jobTitleId;
}
