package com.evotek.order.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.IdUtils;
import com.evotek.order.domain.command.MenuCreateCmd;
import com.evotek.order.domain.command.MenuProductCmd;
import com.evotek.order.domain.command.MenuUpdateCmd;
import com.evotek.order.domain.command.PurchaseOrderCreateCmd;
import com.evotek.order.infrastructure.support.enums.MenuType;
import com.evotek.order.infrastructure.support.enums.PurchaseOrderType;
import com.evotek.order.infrastructure.support.exception.BadRequestError;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.Getter;
import lombok.experimental.SuperBuilder;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
@Setter
@Getter
@SuperBuilder
public class Menu extends AuditableDomain {
    private String id;
    private String code;
    private String title;
    private BigDecimal price;
    private BigDecimal maxTotalPricePurchaseOrder;
    private Instant closedAt;
    private MenuType type;
    private String note;
    private Boolean published = Boolean.FALSE;
    private Boolean deleted;
    private List<MenuProduct> menuProducts = new ArrayList<>();
    private List<Product> products = new ArrayList<>();

    public Menu(MenuCreateCmd cmd, List<Product> products) {
        this.id = IdUtils.nextId();
        this.code = cmd.getCode();
        this.title = cmd.getTitle();
        this.closedAt = cmd.getClosedAt();
        this.type = cmd.getType();
        updatePrice(cmd.getPrice(), cmd.getMaxTotalPricePurOrder());
        this.note = cmd.getNote();
        if(Objects.nonNull(cmd.getPublished())){
            this.published = cmd.getPublished();
        }
        this.deleted = Boolean.FALSE;
        updateMenuProduct(cmd.getMenuProductCmds(), products);
    }

    public void updateMenu(MenuUpdateCmd cmd, List<Product> products, List<PurchaseOrder> purchaseOrders) {
        if (Boolean.TRUE.equals(this.published)) {
            throw new ResponseException(BadRequestError.MENU_USING);
        }
        if (!CollectionUtils.isEmpty(purchaseOrders)) {
            throw new ResponseException(BadRequestError.MENU_USING);
        }
        this.title = cmd.getTitle();
        this.closedAt = cmd.getClosedAt();
        updatePrice(cmd.getPrice(), cmd.getMaxTotalPricePurOrder());
        this.note = cmd.getNote();
        updateMenuProduct(cmd.getMenuProductCmds(), products);
    }

    public void delete() {
        if (Boolean.TRUE.equals(this.published)) {
            throw new ResponseException(BadRequestError.MENU_USING);
        }
        if (!CollectionUtils.isEmpty(this.menuProducts)) {
            this.menuProducts.forEach(MenuProduct::delete);
        }
        this.deleted = Boolean.TRUE;
    }

    public void publishMenu() {
        if (Boolean.TRUE.equals(this.published)) {
            throw new ResponseException(BadRequestError.MENU_ALREADY_PUBLISHED_BEFORE);
        }
        if (Boolean.TRUE.equals(this.closedAt.isAfter(Instant.now()))) {
            this.published = Boolean.TRUE;
        } else {
            throw new ResponseException(BadRequestError.MENU_NOT_ACTIVE);
        }
    }

    public void unPublishMenu() {
        if (Boolean.FALSE.equals(this.published)) {
            throw new ResponseException(BadRequestError.MENU_ALREADY_UNPUBLISHED_BEFORE);
        }
        this.published = Boolean.FALSE;
    }

    private void updateMenuProduct(List<MenuProductCmd> menuProductCmds, List<Product> products) {
        if (!Objects.nonNull(this.products)) {
            this.products = new ArrayList<>();
        }
        if (!Objects.nonNull(this.menuProducts)) {
            this.menuProducts = new ArrayList<>();
        }
        if (!CollectionUtils.isEmpty(this.menuProducts)) {
            this.menuProducts.forEach(MenuProduct::delete);
        }
        for (MenuProductCmd menuProductCmd : menuProductCmds) {
            Product productOfMenu = products.stream()
                    .filter(product -> product.getId().equals(menuProductCmd.getProductId()))
                    .findFirst()
                    .orElseThrow(() -> new ResponseException(BadRequestError.PRODUCT_NOT_FOUND));
            this.menuProducts.add(new MenuProduct(menuProductCmd, this.id));
            this.products.add(productOfMenu);
        }
    }

    private void updatePrice(BigDecimal price, BigDecimal maxTotalPricePurOrder) {
        if (MenuType.BUFFET.equals(this.type)) {
            this.maxTotalPricePurchaseOrder = BigDecimal.ZERO;
            if (!Objects.nonNull(price)) {
                throw new ResponseException(BadRequestError.MENU_MUST_HAVE_PRICE);
            }
            this.price = price;
        } else {
            this.price = BigDecimal.ZERO;
            this.maxTotalPricePurchaseOrder = maxTotalPricePurOrder;
        }
    }

    public void enrichProducts(List<Product> products) {
        this.products = products;
    }

    public void enrichMenuProduct(List<MenuProduct> menuProducts) {
        this.menuProducts = menuProducts;
    }

    public PurchaseOrder createPurchaseOrder(PurchaseOrderCreateCmd cmd) {
        ensurePublished();
        if ((PurchaseOrderType.ORDER_SET.equals(cmd.getType()) && MenuType.SET.equals(this.type))
                || (PurchaseOrderType.ORDER_BUFFET.equals(cmd.getType()) && MenuType.BUFFET.equals(this.type))) {
            return new PurchaseOrder(cmd, this);
        } else {
            throw new ResponseException(BadRequestError.MENU_NOT_SAME_TYPE_WITH_PURCHASE_ORDER);
        }
    }

    public void ensurePublished() {
        if (Boolean.FALSE.equals(this.published) || this.getClosedAt().isBefore(Instant.now())) {
            throw new ResponseException(BadRequestError.MENU_NOT_ACTIVE);
        }
    }
}
