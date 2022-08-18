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

@Entity
@Table(name = "menu_product")
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class MenuProductEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "menu_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String menuId;

    @Column(name = "product_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String productId;

    @Column(name = "max_product_purchase_order")
    @Range(min = 1)
    private Integer maxProductPurchaseOrder;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;
}
