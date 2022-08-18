package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.enums.Gender;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.time.LocalDate;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class UserCreateRequest extends Request {

    @NotBlank(message = "USERNAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "USERNAME_LENGTH")
    @Pattern(regexp = "^[A-Za-z0-9._]+$", message = "USERNAME_FORMAT")
    private String username;

    @NotBlank(message = "FULL_NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "FULL_NAME_LENGTH")
    private String fullName;

    @NotBlank(message = "EMAIL_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.EMAIL_MAX_LENGTH, message = "EMAIL_LENGTH")
    @Pattern(regexp = ValidateConstraint.FORMAT.EMAIL_PATTERN, message = "EMAIL_WRONG_FORMAT")
    private String email;

    @NotBlank(message = "PHONE_NUMBER_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.PHONE_MAX_LENGTH, message = "PHONE_NUMBER_LENGTH")
    @Pattern(regexp = ValidateConstraint.FORMAT.PHONE_NUMBER_PATTERN, message = "PHONE_NUMBER_FORMAT")
    private String phoneNumber;

    @NotNull(message = "ROLE_IDS_REQUIRED")
    private List<String> roleIds;

    private Gender gender;

    private LocalDate dayOfBirth;

    @Size(min = ValidateConstraint.LENGTH.ID_MIN_LENGTH,
            max = ValidateConstraint.LENGTH.ID_MAX_LENGTH, message = "ID_LENGTH")
    private String avatarFileId;

    @Size(max = ValidateConstraint.LENGTH.DESC_MAX_LENGTH, message = "DESC_LENGTH")
    private String description;


    @Pattern(regexp = "^[A-Za-z0-9]+$", message = "FORMAT_EMPLOYEE_CODE")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "EMPLOYEE_CODE_LENGTH")
    private String employeeCode;

    private UserStatus status;

    @Size(max = ValidateConstraint.LENGTH.ID_MAX_LENGTH, message = "JOB_TITLE_ID")
    private String jobTitleId;

    @Valid
    private EmploymentCreateOrUpdateRequest employment;
}
