package com.evotek.order.domain.command;

import lombok.Data;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.List;

@Data
public class MenuUpdateCmd {
    private String title;
    private BigDecimal price;
    private Instant closedAt;
    private String note;
    private BigDecimal maxTotalPricePurOrder;
    private List<MenuProductCmd> menuProductCmds;
}
