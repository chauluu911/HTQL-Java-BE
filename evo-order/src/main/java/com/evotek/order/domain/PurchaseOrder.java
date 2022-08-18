package com.evotek.order.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.IdUtils;
import com.evotek.order.domain.command.PurchaseOrderCreateCmd;
import com.evotek.order.domain.command.PurchaseOrderItemCmd;
import com.evotek.order.domain.command.PurchaseOrderUpdateCmd;
import com.evotek.order.infrastructure.support.enums.PurchaseOrderStatus;
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
public class PurchaseOrder extends AuditableDomain {
    private String id;
    private String createdUserId;
    private String ownerId;
    private String createdUserFullName;
    private String ownerFullName;
    private String menuId;
    private String code;
    private List<PurchaseOrderItem> purchaseOrderItems = new ArrayList<>();
    private BigDecimal totalPrice = BigDecimal.ZERO;
    private Integer totalQuantity = 0;
    private PurchaseOrderStatus status;
    private PurchaseOrderType type;
    private Instant paymentAt;
    private Boolean deleted;
    private List<PurchaseOrderHistory> purchaseOrderHistories = new ArrayList<>();

    public PurchaseOrder(PurchaseOrderCreateCmd cmd, Menu menu) {
        this.id = IdUtils.nextId();
        this.code = cmd.getCode();
        this.menuId = cmd.getMenuId();
        this.createdUserId = cmd.getCreatedUserId();
        this.ownerId = cmd.getOwnerId();
        this.type = cmd.getType();
        updateHistory(PurchaseOrderStatus.NEW, this.ownerId);
        updatePurchaseOrderItemsData(cmd.getPurchaseOrderItems(), menu.getProducts());
        calculatePriceAndQuantity(menu);
        this.deleted = Boolean.FALSE;
    }

    public void updatePurchaseOrder(PurchaseOrderUpdateCmd cmd, Menu menu) {
        if (!PurchaseOrderStatus.NEW.equals(this.status)) {
            throw new ResponseException(BadRequestError.PURCHASE_ORDER_CAN_NOT_UPDATE);
        }
        menu.ensurePublished();
        updatePurchaseOrderItemsData(cmd.getPurchaseOrderItems(), menu.getProducts());
        calculatePriceAndQuantity(menu);
    }

    public void delete() {
        this.purchaseOrderItems.forEach(PurchaseOrderItem::delete);
        this.deleted = Boolean.TRUE;
    }

    private void updatePurchaseOrderItemsData(List<PurchaseOrderItemCmd> purchaseOrderItemCmds, List<Product> productOfMenus) {
        if (CollectionUtils.isEmpty(purchaseOrderItemCmds)) {
            throw new ResponseException(BadRequestError.PURCHASE_ORDER_NOT_DATA);
        }
        if (!Objects.nonNull(this.purchaseOrderItems)) {
            this.purchaseOrderItems = new ArrayList<>();
        } else {
            this.purchaseOrderItems.forEach(PurchaseOrderItem::delete);
        }
        for (PurchaseOrderItemCmd cmd : purchaseOrderItemCmds) {
            Product productOfOrderItem = productOfMenus.stream()
                    .filter(product -> product.getId().equals(cmd.getProductId()))
                    .findFirst()
                    .orElseThrow(() -> new ResponseException(BadRequestError.PRODUCT_NOT_FOUND));
            this.purchaseOrderItems.add(new PurchaseOrderItem(cmd, this.id, productOfOrderItem));
        }
    }

    private void calculatePriceAndQuantity(Menu menu) {
        this.totalPrice = BigDecimal.ZERO;
        this.totalQuantity = 0;
        if (PurchaseOrderType.ORDER_BUFFET.equals(this.type)) {
            this.totalPrice = menu.getPrice();
            this.totalQuantity = 1;
        } else {
            if (CollectionUtils.isEmpty(this.purchaseOrderItems)) {
                this.purchaseOrderItems = new ArrayList<>();
            }

            this.purchaseOrderItems.forEach(orderItem -> {
                if (Boolean.FALSE.equals(orderItem.getDeleted())) {
                    this.totalPrice = this.totalPrice
                            .add(BigDecimal
                                    .valueOf(orderItem.getQuantity()).multiply(orderItem.getProductPrice()));
                    this.totalQuantity += orderItem.getQuantity();
                }
            });
        }
    }

    public void enrichPurchaseOrderItems(List<PurchaseOrderItem> purchaseOrderItemsExisted) {
        this.purchaseOrderItems = purchaseOrderItemsExisted;
    }

    public void acceptOrderStatus(String userId) {
        if (PurchaseOrderStatus.NEW.equals(this.status)) {
            updateHistory(PurchaseOrderStatus.ORDER, userId);
        } else if (PurchaseOrderStatus.ORDER.equals(this.status)) {
            // do nothing
        } else {
            throw new ResponseException(BadRequestError.PURCHASE_ORDER_STATUS_MUST_IS_NEW);
        }
    }

    public void acceptDeliveredStatus(String userId) {
        if (PurchaseOrderStatus.ORDER.equals(this.status)) {
            updateHistory(PurchaseOrderStatus.DELIVERED, userId);
        } else {
            throw new ResponseException(BadRequestError.PURCHASE_ORDER_STATUS_MUST_IS_ORDER);
        }
    }

    public void acceptPaymentStatus(String userId) {
        if (PurchaseOrderStatus.ORDER.equals(this.status)) {
            updateHistory(PurchaseOrderStatus.PAID, userId);
            this.paymentAt = Instant.now();
        } else if (PurchaseOrderStatus.DELIVERED.equals(this.status)) {
            // do nothing
        } else {
            throw new ResponseException(BadRequestError.PURCHASE_ORDER_STATUS_MUST_IS_DELIVERED_OR_ORDER);
        }
    }

    public void revertOrderStatus(String userId) {
        if (PurchaseOrderStatus.PAID.equals(this.status)) {
            updateHistory(PurchaseOrderStatus.ORDER, userId);
        } else {
            throw new ResponseException(BadRequestError.PURCHASE_ORDER_STATUS_MUST_BE_PAID);
        }
    }

    public void enrichHistory(List<PurchaseOrderHistory> purchaseOrderHistories) {
        this.purchaseOrderHistories = purchaseOrderHistories;
    }

    private void updateHistory(PurchaseOrderStatus status, String userId) {
        if (CollectionUtils.isEmpty(this.purchaseOrderHistories)) {
            this.purchaseOrderHistories = new ArrayList<>();
        }
        this.status = status;
        this.purchaseOrderHistories.add(new PurchaseOrderHistory(userId, this.id, status));
    }

    public void enrichCreatedUserFullName(String userFullName) {
        this.createdUserFullName = userFullName;
    }

    public void enrichOwnerFullName(String ownerFullName) {
        this.ownerFullName = ownerFullName;
    }
}
