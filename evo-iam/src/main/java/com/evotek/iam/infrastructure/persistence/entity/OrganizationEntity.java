package com.evotek.iam.infrastructure.persistence.entity;
import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.OrganizationStatus;
import com.evotek.iam.infrastructure.support.enums.OrganizationType;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.time.LocalDate;
import java.util.Objects;

@Entity
@Table(name = "organization", indexes = {
        @Index(name = "organization_business_code_idx", columnList = "business_code"),
        @Index(name = "organization_deleted_idx", columnList = "deleted")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class OrganizationEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name="code", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, nullable = false)
    private String code;

    @Column(name = "name", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH)
    private String name;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Column(name = "status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private OrganizationStatus status = OrganizationStatus.ACTIVE;

    @Column(name = "type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private OrganizationType type;

    @Column(name = "incorporation_date")
    private LocalDate incorporationDate;

    @Column(name = "phone_number", length = ValidateConstraint.LENGTH.PHONE_MAX_LENGTH, nullable = false)
    private String phoneNumber;

    @Column(name = "email", length = ValidateConstraint.LENGTH.EMAIL_MAX_LENGTH, nullable = false)
    private String email;

    @Column(name = "legal_representative", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, nullable = false)
    private String legalRepresentative;

    @Column(name = "business_code", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH)
    private String businessCode;

    @Column(name = "invoice_issuing_address", length = ValidateConstraint.LENGTH.ADDRESS_MAX_LENGTH)
    private String invoiceIssuingAddress;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        OrganizationEntity that = (OrganizationEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }

}
