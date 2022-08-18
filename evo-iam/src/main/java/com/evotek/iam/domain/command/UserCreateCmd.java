package com.evotek.iam.domain.command;

import com.evotek.common.enums.Gender;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;

@JsonInclude(JsonInclude.Include.NON_NULL)
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class UserCreateCmd {

    private String username;
    private String fullName;
    private String email;
    private String phoneNumber;
    private LocalDate dayOfBirth;
    private Gender gender;
    private String description;
    private UserStatus status;
    private String avatarFileId;
    private String avatarFileUrl;
    private EmploymentCreateOrUpdateCmd employment;
    private String employeeCode;
    private String departmentId;
    private String jobTitleId;
}
