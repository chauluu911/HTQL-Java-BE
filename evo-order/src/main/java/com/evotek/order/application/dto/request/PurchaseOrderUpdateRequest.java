package com.evotek.order.application.dto.request;

import com.evotek.common.dto.request.Request;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.List;

@Data
@EqualsAndHashCode(callSuper = true)
public class PurchaseOrderUpdateRequest extends Request {
    @NotNull(message = "PURCHASE_ORDER_ITEMS_REQUIRED")
    @Size(min = 1, message = "ORDER_ITEM_MIN_LENGTH")
    private List<PurchaseOrderItemRequest> purchaseOrderItems;
}
