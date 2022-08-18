package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.enums.Gender;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.time.LocalDate;

@EqualsAndHashCode(callSuper = true)
@Data
public class UserUpdateProfileRequest extends Request {

    @NotBlank(message = "FULL_NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "FULL_NAME_LENGTH")
    private String fullName;

    @NotBlank(message = "EMAIL_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "EMAIL_LENGTH")
    @Pattern(regexp = ValidateConstraint.FORMAT.EMAIL_PATTERN, message = "EMAIL_WRONG_FORMAT")
    private String email;

    @NotBlank(message = "PHONE_NUMBER_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "PHONE_NUMBER_LENGTH")
    private String phoneNumber;

    private String avatarFileId;
    private Gender gender;
    private LocalDate dayOfBirth;
}
