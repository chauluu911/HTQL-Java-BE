package com.evotek.order.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.Range;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.Instant;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class MenuUpdateRequest extends Request {
    @NotBlank(message = "TITLE_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.TITLE_MAX_LENGTH, message = "TITLE_LENGTH")
    private String title;

    @Range(min = 0, message = "PRICE_MIN")
    private BigDecimal price;

    private BigDecimal maxTotalPricePurOrder;

    @NotNull(message = "CLOSE_TIME_REQUIRED")
    private Instant closedAt;

    @Size(max = ValidateConstraint.LENGTH.NOTE_MAX_LENGTH, message = "MENU_LENGTH")
    private String note;

    @NotNull(message = "MENU_PRODUCT_REQUIRED")
    @Size(min = 1, message = "MENU_PRODUCT_MIN_SIZE")
    private List<MenuProductRequest> menuProductRequests;

}
