package com.evotek.order.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import com.evotek.order.infrastructure.support.enums.ProductType;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.math.BigDecimal;

@EqualsAndHashCode(callSuper = true)
@Data
public class ProductSearchRequest extends PagingRequest {
    private String name;
    private BigDecimal startPrice;
    private BigDecimal endPrice;
    private ProductType type;
    private String keyword;
}
