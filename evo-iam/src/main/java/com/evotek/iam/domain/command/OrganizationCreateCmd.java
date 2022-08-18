package com.evotek.iam.domain.command;

import com.evotek.iam.infrastructure.support.enums.OrganizationStatus;
import com.evotek.iam.infrastructure.support.enums.OrganizationType;
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
public class OrganizationCreateCmd {
    private String name;
    private String code;
    private OrganizationType type;
    private LocalDate incorporationDate;
    private String phoneNumber;
    private String email;
    private String legalRepresentative;
    private String businessCode;
    private String invoiceIssuingAddress;
    private OrganizationStatus status;
}
