package com.evotek.order.infrastructure.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.validator.constraints.Range;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.math.BigDecimal;

@Entity
@Table(name = "vendor")
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class VendorEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "name", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, nullable = false)
    private String name;

    @Column(name = "phone", length = ValidateConstraint.LENGTH.PHONE_MAX_LENGTH, nullable = false)
    private String phone;

    @Column(name = "email", length = ValidateConstraint.LENGTH.EMAIL_MAX_LENGTH, nullable = false)
    private String email;

    @Column(name = "address", length = ValidateConstraint.LENGTH.ADDRESS_MAX_LENGTH, nullable = false)
    private String address;

    @Column(name = "bank_name", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, nullable = false)
    private String bankName;

    @Column(name = "bank_number", length = ValidateConstraint.LENGTH.PHONE_MAX_LENGTH, nullable = false)
    private String bankNumber;

    @Column(name = "price", nullable = false)
    @Range(min = 0)
    private BigDecimal price;

    @Column(name = "number_salty_food", nullable = false)
    private Integer numberOfSaltyFood;

    @Column(name = "number_balend_food", nullable = false)
    private Integer numberOfBalendFood;
}
