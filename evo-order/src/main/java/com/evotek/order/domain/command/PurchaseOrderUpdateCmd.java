package com.evotek.order.domain.command;

import lombok.Data;

import java.util.List;

@Data
public class PurchaseOrderUpdateCmd {
    private List<PurchaseOrderItemCmd> purchaseOrderItems;
}
