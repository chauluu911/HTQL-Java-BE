package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.time.LocalDate;

@EqualsAndHashCode(callSuper = true)
@Data
public class EmploymentCreateOrUpdateRequest extends Request {
    @NotBlank(message = "DEPARTMENT_ID_REQUIRED")
    private String departmentId;

    private LocalDate startDate;

    private LocalDate endDate;

    @Size(max = ValidateConstraint.LENGTH.DESC_MAX_LENGTH, message = "JOB_TITLE_ID")
    private String jobTitleId;

    private Boolean manager;
}
