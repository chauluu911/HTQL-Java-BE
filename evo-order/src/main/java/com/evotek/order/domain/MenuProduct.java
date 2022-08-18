package com.evotek.order.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import com.evotek.order.domain.command.MenuProductCmd;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.Getter;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
@Setter
@Getter
@SuperBuilder
public class MenuProduct extends AuditableDomain {
    private String id;
    private String menuId;
    private String productId;
    private Integer maxProductPurchaseOrder;
    private Boolean deleted;

    public MenuProduct(MenuProductCmd cmd, String menuId) {
        this.id = IdUtils.nextId();
        this.menuId = menuId;
        this.productId = cmd.getProductId();
        this.maxProductPurchaseOrder = cmd.getMaxProductPurchaseOrder();
        this.deleted = Boolean.FALSE;
    }

    public void delete() {
        this.deleted = Boolean.TRUE;
    }
}
