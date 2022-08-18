package com.evotek.iam.domain.command;

import com.evotek.iam.infrastructure.support.enums.OrganizationStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class OrganizationUpdateCmd {
    private String name;
    private String phoneNumber;
    private String email;
    private String legalRepresentative;
    private String invoiceIssuingAddress;
    private LocalDate incorporationDate;
    private OrganizationStatus status;
}

