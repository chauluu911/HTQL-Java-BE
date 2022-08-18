package com.evotek.order.domain.command;

import com.evotek.order.infrastructure.support.enums.ProductSubType;
import com.evotek.order.infrastructure.support.enums.ProductType;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class ProductUpdateCmd {
    private String name;
    private BigDecimal price;
    private ProductType type;
    private ProductSubType subType;
    private String imageId;
}
