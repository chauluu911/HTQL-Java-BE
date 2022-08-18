package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.OrganizationStatus;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.time.LocalDate;

@EqualsAndHashCode(callSuper = true)
@Data
public class OrganizationUpdateRequest extends Request {

    @NotBlank(message = "ORG_NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "ORG_NAME_LENGTH")
    private String name;

    @NotBlank(message = "PHONE_NUMBER_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.PHONE_MAX_LENGTH, message = "PHONE_NUMBER_LENGTH")
    @Pattern(regexp = ValidateConstraint.FORMAT.PHONE_NUMBER_PATTERN, message = "PHONE_NUMBER_FORMAT")
    private String phoneNumber;

    @NotBlank(message = "EMAIL_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.EMAIL_MAX_LENGTH, message = "EMAIL_LENGTH")
    @Pattern(regexp = ValidateConstraint.FORMAT.EMAIL_PATTERN, message = "EMAIL_WRONG_FORMAT")
    private String email;

    @NotBlank(message = "LEGAL_REPRESENTATIVE_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "LEGAL_REPRESENTATIVE_MAX_LENGTH")
    private String legalRepresentative;

    @Size(max = ValidateConstraint.LENGTH.ADDRESS_MAX_LENGTH, message = "INVOICE_ISSUING_ADDRESS_MAX_LENGTH")
    private String invoiceIssuingAddress;

    @NotNull(message = "ORG_STATUS_REQUIRED")
    private OrganizationStatus status;

    private LocalDate incorporationDate;
}

