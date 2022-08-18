package com.evotek.iam.domain;

import lombok.*;
import lombok.experimental.SuperBuilder;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

@EqualsAndHashCode(callSuper = false)
@Data
@SuperBuilder
@NoArgsConstructor
@Setter(AccessLevel.PRIVATE)
@Getter
@Slf4j
public class Employee extends User {
    private JobTitle jobTitle;
    private List<EmploymentHistory> employmentHistories;
    private String companyCode;

    public void enrichJobTitle(JobTitle jobTitle) {
        this.jobTitle = jobTitle;
    }

    public void enrichEmploymentHistories(List<EmploymentHistory> employmentHistories) {
        this.employmentHistories = employmentHistories;
    }

    public void enrichCompanyCode(String companyCode) {
        this.companyCode = companyCode;
    }
}
