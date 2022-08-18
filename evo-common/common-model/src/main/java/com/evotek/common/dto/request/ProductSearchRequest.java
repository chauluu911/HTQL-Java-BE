package com.evotek.common.dto.request;

import java.math.BigDecimal;

public class ProductSearchRequest extends PagingRequest{
    private String name;
    private BigDecimal startPrice;
    private BigDecimal endPrice;
    private String keyword;
}
