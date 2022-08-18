package com.evotek.iam.domain.command;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class EmploymentCreateOrUpdateCmd {

    private String departmentId;

    private LocalDate startDate;

    private LocalDate endDate;

    private String jobTitleId;

    private Boolean isManager;
}
