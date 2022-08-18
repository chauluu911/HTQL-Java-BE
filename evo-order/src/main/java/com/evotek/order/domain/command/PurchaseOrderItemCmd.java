package com.evotek.order.domain.command;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class PurchaseOrderItemCmd {
    private String productId;
    private Integer quantity;
}
