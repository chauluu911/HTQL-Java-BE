package com.evotek.order.domain.command;

import com.evotek.order.infrastructure.support.enums.PurchaseOrderType;
import lombok.Data;

import java.util.List;

@Data
public class PurchaseOrderCreateCmd {
    private String code;
    private String createdUserId;
    private String ownerId;
    private PurchaseOrderType type;
    private String menuId;
    private List<PurchaseOrderItemCmd> purchaseOrderItems;

    public void enrichCode(String code) {
        this.code = code;
    }

    public void enrichOwnerId(String ownerId) {
        this.ownerId = ownerId;
    }
}
