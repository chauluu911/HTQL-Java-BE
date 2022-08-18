package com.evotek.order.infrastructure.query;

import com.evotek.common.query.PagingQuery;
import com.evotek.order.infrastructure.support.enums.PurchaseOrderStatus;
import com.evotek.order.infrastructure.support.enums.PurchaseOrderType;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

import java.time.Instant;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class PurchaseOrderSearchQuery extends PagingQuery {
    private PurchaseOrderType type;
    private PurchaseOrderStatus status;
    private Instant startCreatedAt;
    private Instant endCreatedAt;
    private String userId;
    private String menuId;
    private String createdUserId;
}
