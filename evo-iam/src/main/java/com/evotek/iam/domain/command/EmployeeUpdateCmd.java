package com.evotek.iam.domain.command;

import com.evotek.common.enums.Gender;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import lombok.Data;

import java.time.LocalDate;

@Data
public class EmployeeUpdateCmd {
    private String fullName;
    private String email;
    private String phoneNumber;
    private LocalDate dayOfBirth;
    private Gender gender;
    private String title;
    private String description;
    private UserStatus status;
    private String departmentName;
    private String departmentId;
    private String avatarFileId;
    private String jobTitleId;
}
