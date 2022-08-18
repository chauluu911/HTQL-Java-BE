package com.evotek.iam.domain.command;

import com.evotek.common.enums.Gender;
import com.evotek.iam.infrastructure.support.enums.AuthenticationType;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserUpdateCmd {

    private String password;
    private String fullName;
    private String email;
    private String phoneNumber;
    private AuthenticationType authenticationType;
    private LocalDate dayOfBirth;
    private Gender gender;
    private List<String> roleIds;
    private String employeeCode;
    private String title;
    private String description;
    private UserStatus status;
    private String avatarFileId;
    private String jobTitleId;
    private EmploymentCreateOrUpdateCmd employment;
}
