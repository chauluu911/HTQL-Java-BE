package com.evotek.order.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
public class PurchaseOrderItemRequest extends Request {
    @NotNull(message = "PRODUCT_ID_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.ID_MAX_LENGTH, message = "ID_MAX_LENGTH")
    private String productId;

    private Integer quantity;
}
