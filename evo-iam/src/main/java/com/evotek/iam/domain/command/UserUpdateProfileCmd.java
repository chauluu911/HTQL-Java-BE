package com.evotek.iam.domain.command;

import com.evotek.common.enums.Gender;
import lombok.Data;

import java.time.LocalDate;

@Data
public class UserUpdateProfileCmd {

    private String fullName;
    private String email;
    private String phoneNumber;
    private LocalDate dayOfBirth;
    private Gender gender;
    private String avatarFileId;
    private String jobTitleId;
}
