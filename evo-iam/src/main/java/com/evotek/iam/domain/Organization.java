package com.evotek.iam.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.IdUtils;
import com.evotek.iam.domain.command.*;
import com.evotek.iam.infrastructure.support.enums.OrganizationStatus;
import com.evotek.iam.infrastructure.support.enums.OrganizationType;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;
import java.util.Date;
import java.util.List;
import java.util.Objects;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class Organization extends AuditableDomain {
    private String id;
    private String code;
    private String name;
    private Boolean deleted;
    private OrganizationStatus status;
    private OrganizationType type;
    private LocalDate incorporationDate;
    private String phoneNumber;
    private String email;
    private String legalRepresentative;
    private String businessCode;
    private String invoiceIssuingAddress;
    private List<String> buildingIds;

    public Organization(OrganizationCreateCmd cmd) {
        this.id = IdUtils.nextId();
        this.name = cmd.getName();
        if (Objects.isNull(cmd.getType())) {
            this.type = OrganizationType.INTERNAL_CUSTOMER;
        } else {
            this.type = cmd.getType();
        }
        this.incorporationDate = cmd.getIncorporationDate();
        this.phoneNumber = cmd.getPhoneNumber();
        this.email = cmd.getEmail();
        this.legalRepresentative = cmd.getLegalRepresentative();
        this.businessCode = cmd.getBusinessCode();
        this.invoiceIssuingAddress = cmd.getInvoiceIssuingAddress();
        this.deleted = false;
        this.status = Objects.nonNull(cmd.getStatus()) ? cmd.getStatus() : OrganizationStatus.ACTIVE;
    }

    public void update(OrganizationUpdateCmd cmd) {
        this.name = cmd.getName();
        this.phoneNumber = cmd.getPhoneNumber();
        this.email = cmd.getEmail();
        this.legalRepresentative = cmd.getLegalRepresentative();
        this.invoiceIssuingAddress = cmd.getInvoiceIssuingAddress();
        this.incorporationDate = cmd.getIncorporationDate();
        if (Objects.nonNull(cmd.getStatus())) {
            this.status = cmd.getStatus();
        }
    }

    public void active() {
        this.status = OrganizationStatus.ACTIVE;
    }

    public void inactive() {
        this.status = OrganizationStatus.INACTIVE;
    }

    public void deleted() {
        if (Boolean.TRUE.equals(this.deleted)) {
            throw new ResponseException(BadRequestError.CANNOT_DELETE_DELETED_ORGANIZATION);
        }
        this.deleted = true;
    }

    public void enrichCode(String code) {
        this.code = code;
    }
}
