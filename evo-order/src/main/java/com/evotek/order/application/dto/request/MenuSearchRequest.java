package com.evotek.order.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import com.evotek.order.infrastructure.support.enums.MenuType;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.time.Instant;

@EqualsAndHashCode(callSuper = true)
@Data
public class MenuSearchRequest extends PagingRequest {
    private String title;
    private MenuType type;
    private Boolean published;
    private Instant startClosedAt;
    private Instant endClosedAt;
    private String keyword;
}
