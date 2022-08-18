package com.evotek.order.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.order.infrastructure.support.enums.PurchaseOrderType;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class PurchaseOrderCreateRequest extends Request {
    @NotNull(message = "CREATED_USER_ID_REQUIRED")
    private String createdUserId;

    @NotNull(message = "MENU_ID_REQUIRED")
    private String menuId;

    @NotNull(message = "TYPE_REQUIRED")
    private PurchaseOrderType type;

    @NotNull(message = "PURCHASE_ORDER_ITEMS_REQUIRED")
    @Size(min = 1, message = "PURCHASE_ORDER_ITEMS_MIN")
    private List<PurchaseOrderItemRequest> purchaseOrderItems;
}
