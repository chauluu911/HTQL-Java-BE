package com.evotek.order.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import com.evotek.order.domain.command.PurchaseOrderItemCmd;
import com.evotek.order.infrastructure.support.enums.ProductType;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.Getter;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;

@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
@Setter
@Getter
@SuperBuilder
public class PurchaseOrderItem extends AuditableDomain {
    private String id;
    private String purchaseOrderId;
    private String productId;
    private String productName;
    private BigDecimal productPrice;
    private Integer quantity;
    private Boolean deleted;

    public PurchaseOrderItem(PurchaseOrderItemCmd cmd, String purchaseOrderId, Product product) {
        this.id = IdUtils.nextId();
        this.purchaseOrderId = purchaseOrderId;
        this.productId = cmd.getProductId();
        this.productName = product.getName();
        this.productPrice = product.getPrice();
        if (ProductType.LUNCH.equals(product.getType())) {
            this.quantity = 1;
        } else {
            this.quantity = cmd.getQuantity();
        }
        this.deleted = Boolean.FALSE;
    }

    public void delete() {
        this.deleted = Boolean.TRUE;
    }
}
