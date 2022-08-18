package com.evotek.iam.application.dto.response;

import com.evotek.common.enums.Gender;
import com.evotek.common.enums.UserLevel;
import com.evotek.iam.domain.EmploymentHistory;
import com.evotek.iam.domain.JobTitle;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;

@JsonInclude(JsonInclude.Include.NON_NULL)
@Data
public class EmployeeDTO {
    private String id;
    private String fullName;
    private String email;
    private String phoneNumber;
    private String username;
    private LocalDate dayOfBirth;
    private Gender gender;
    private Boolean deleted;
    private String employeeCode;
    private String title;
    private String description;
    private UserStatus status;
    private String departmentName;
    private JobTitle jobTitle;
    private UserLevel employeeLevel;
    private String avatarFileId;
    private String avatarFileUrl;
    private String departmentId;
    private List<EmploymentHistory> employmentHistories;
    private String companyCode;
}
