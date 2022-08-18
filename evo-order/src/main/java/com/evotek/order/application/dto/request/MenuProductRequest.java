package com.evotek.order.application.dto.request;

import com.evotek.common.dto.request.Request;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.Range;

import javax.validation.constraints.NotNull;

@EqualsAndHashCode(callSuper = true)
@Data
public class MenuProductRequest extends Request {
    @NotNull(message = "PRODUCT_ID_REQUIRED")
    private String productId;

    @NotNull(message = "MAX_PRODUCT_PURCHASE_ORDER")
    @Range(min = 1, message = "PRODUCT_PURCHASE_ORDER_MIN")
    private Integer maxProductPurchaseOrder;
}
