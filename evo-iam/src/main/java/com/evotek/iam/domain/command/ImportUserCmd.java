package com.evotek.iam.domain.command;

import com.evotek.common.enums.Gender;
import com.evotek.iam.infrastructure.support.enums.AuthenticationType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;

@Data
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class ImportUserCmd extends UserCreateCmd {
    private String employeeCode;
    private String fullName;
    private String departmentName;
    private String departmentId;
    private LocalDate dayOfBirth;
    private Gender gender;
    private String phoneNumber;
    private String email;
    private AuthenticationType authenticationType;
    private String password;
}
