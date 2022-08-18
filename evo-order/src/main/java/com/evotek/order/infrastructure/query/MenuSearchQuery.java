package com.evotek.order.infrastructure.query;

import com.evotek.common.query.PagingQuery;
import com.evotek.order.infrastructure.support.enums.MenuType;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

import java.time.Instant;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class MenuSearchQuery extends PagingQuery {
    private String title;
    private MenuType type;
    private Boolean published;
    private Instant startClosedAt;
    private Instant endClosedAt;
}
