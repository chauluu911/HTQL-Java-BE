package com.evotek.order.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.order.infrastructure.support.enums.ProductSubType;
import com.evotek.order.infrastructure.support.enums.ProductType;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.Range;

import javax.validation.constraints.NotNull;
import java.math.BigDecimal;

@EqualsAndHashCode(callSuper = true)
@Data
public class ProductUpdateRequest extends Request {
    @NotNull(message = "NAME_REQUIRED")
    private String name;

    private BigDecimal price;

    @NotNull(message = "TYPE_REQUIRED")
    private ProductType type;

    private ProductSubType subType;

    private String imageId;
}
