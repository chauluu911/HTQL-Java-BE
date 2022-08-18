package com.evotek.order.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import com.evotek.order.infrastructure.support.enums.PurchaseOrderStatus;
import lombok.experimental.SuperBuilder;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.Getter;

@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
@Setter
@Getter
@SuperBuilder
public class PurchaseOrderHistory extends AuditableDomain {
    private String id;
    private String createdUserId;
    private String createdUserFullName;
    private String purchaseOrderId;
    private PurchaseOrderStatus status;
    private Boolean deleted;

    public PurchaseOrderHistory(String createdUserId, String purchaseOrderId, PurchaseOrderStatus status) {
        this.id = IdUtils.nextId();
        this.createdUserId = createdUserId;
        this.purchaseOrderId = purchaseOrderId;
        this.status = status;
        this.deleted = Boolean.FALSE;
    }

    public void enrichCreatedUserFullName(String createdUserFullName) {
        this.createdUserFullName = createdUserFullName;
    }
}
