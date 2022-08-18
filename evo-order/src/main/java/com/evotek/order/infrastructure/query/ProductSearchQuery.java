package com.evotek.order.infrastructure.query;

import com.evotek.common.query.PagingQuery;
import com.evotek.order.infrastructure.support.enums.ProductType;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class ProductSearchQuery extends PagingQuery {
    private String name;
    private BigDecimal startPrice;
    private BigDecimal endPrice;
    private ProductType type;
}
