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
import javax.validation.constraints.Min;
import java.math.BigDecimal;

@Entity
@Table(name = "purchase_order_item")
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class PurchaseOrderItemEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "purchase_order_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String purchaseOrderId;

    @Column(name = "product_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String productId;

    @Min(value = 0)
    @Column(name = "quantity", nullable = false)
    private Integer quantity;

    @Column(name = "product_name", length = ValidateConstraint.LENGTH.PRODUCT_NAME_MAX_LENGTH, nullable = false)
    private String productName;

    @Column(name = "product_price", nullable = false)
    @Range(min = 0)
    private BigDecimal productPrice;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;
}
