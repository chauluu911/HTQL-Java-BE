package com.evotek.order.domain.command;

import com.evotek.order.infrastructure.support.enums.MenuType;
import lombok.Data;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.List;

@Data
public class MenuCreateCmd {
    private String title;
    private String code;
    private BigDecimal price;
    private BigDecimal maxTotalPricePurOrder;
    private List<MenuProductCmd> menuProductCmds;
    private Instant closedAt;
    private String note;
    private MenuType type;
    private Boolean published;

    public void enrichCode(String code) {
        this.code = code;
    }
}
