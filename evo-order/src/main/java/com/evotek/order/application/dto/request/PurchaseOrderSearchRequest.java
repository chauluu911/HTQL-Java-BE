package com.evotek.order.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import com.evotek.order.infrastructure.support.enums.MenuStatus;
import com.evotek.order.infrastructure.support.enums.PurchaseOrderStatus;
import com.evotek.order.infrastructure.support.enums.PurchaseOrderType;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.time.Instant;

@EqualsAndHashCode(callSuper = true)
@Data
public class PurchaseOrderSearchRequest extends PagingRequest {
    private PurchaseOrderType type;
    private PurchaseOrderStatus status;
    private Instant startCreatedAt;
    private Instant endCreatedAt;
    private String userId;
    private String menuId;
}
