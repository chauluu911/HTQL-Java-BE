package com.evotek.order.infrastructure.support.exception;

import com.evotek.common.error.ResponseError;

public enum BadRequestError implements ResponseError {

    FILE_MUST_BE_IMAGE(40006001, "File must be image"),
    FILE_NOT_EXIST(40006002, "File not exist"),
    MENU_NOT_FOUND(40006003, "Menu not found"),
    MENU_NOT_ACTIVE(40006004, "Menu not active"),
    PRODUCT_NOT_FOUND(40006005, "Product not found"),
    PRODUCT_NOT_ACTIVE(40006006, "Product not active"),
    PURCHASE_ORDER_NOT_FOUND(40006007, "PurchaseOrder not found"),
    PURCHASE_ORDER_NOT_ACTIVE(40006008, "PurchaseOrder not active"),
    TIME_IN_OTHER_BILL(40006009, "StartDate or EndDate In Other Bill"),
    USER_NOT_FOUND(40006010, "user not found"),
    MENU_USING_PRODUCT(40006011, "Menu using this product"),
    MENU_NOT_HAVE_PRODUCT(40006012, "Menu not have product"),
    MENU_USING(40006013, "Menu using"),
    PURCHASE_ORDER_NOT_DATA(40006014, "PurchaseOrder not select item"),
    MENU_MUST_HAVE_PRICE(40006015, "Menu must have price"),
    PRODUCT_MUST_HAVE_PRICE(40006016, "Product must have price"),
    MENU_NOT_SAME_TYPE_WITH_PURCHASE_ORDER(40006017, "Menu not same type with purchase order"),
    PURCHASE_ORDER_CAN_NOT_UPDATE(40006019, "Purchase order can't update"),
    PRODUCT_MUST_HAVE_SUBTYPE(40006019, "Product must have subType"),
    MENU_ALREADY_PUBLISHED_BEFORE(40006020, "Menu already published before"),
    MENU_ALREADY_UNPUBLISHED_BEFORE(40006021, "Menu already unPublished before "),
    PURCHASE_ORDER_STATUS_MUST_IS_NEW(40006022, "Purchase order must is NEW, so it can change to ORDER"),
    PURCHASE_ORDER_STATUS_MUST_IS_ORDER(40006023, "Purchase order must is ORDER so it can change to DELIVERED"),
    PURCHASE_ORDER_STATUS_MUST_IS_DELIVERED_OR_ORDER(40006024, "Purchase order must is DELIVERED or ORDER, so it can change to PAID"),
    PURCHASE_ORDER_IS_EMPTY_TO_CHANGE_STATUS(40006025, "Purchase order is empty to change status"),
    PURCHASE_ORDER_STATUS_MUST_BE_PAID(40006026, "Purchase order must be paid, in order to be reverted to order")
    ;

    private final Integer code;
    private final String message;

    BadRequestError(Integer code, String message) {
        this.code = code;
        this.message = message;
    }

    @Override
    public String getName() {
        return name();
    }

    @Override
    public String getMessage() {
        return message;
    }

    @Override
    public int getStatus() {
        return 400;
    }

    @Override
    public Integer getCode() {
        return code;
    }
}
