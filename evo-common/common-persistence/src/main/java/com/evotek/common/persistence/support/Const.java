package com.evotek.common.persistence.support;

public enum Const {
    TICKET_CUSTOMER("YC", "TICKET_CUSTOMER_SEQ"),
    TICKET_INTERNAL("NB", "TICKET_INTERNAL_SEQ"),
    TICKET_COMPLAINT("BN", "TICKET_COMPLAINT_SEQ"),
    ORGANIZATION_INTERNAL("NB", "ORGANIZATION_INTERNAL_SEQ"),
    ORGANIZATION_EXTERNAL("TN", "ORGANIZATION_EXTERNAL_SEQ"),
    UNIT_CODE("MB", "UNIT_CODE"),
    COMPLAINT_CODE("GY", "COMPLAINT_CODE_SEQ"),
    PRODUCT_CODE("PRODUCT", "PRODUCT_CODE_SEQ"),
    MENU_CODE("MENU", "MENU_CODE_SEQ"),
    ORDER_CODE("ORDER", "ORDER_CODE_SEQ"),
    ORGANIZATION_CODE("ORG", "ORGANIZATION_CODE_SEQ"),
    ASSET_CODE("AST", "ASSET_CODE_SEQ");

    private final String prefix;

    // must upper case
    private final String seqName;

    Const(String prefix, String seqName) {
        this.prefix = prefix;
        this.seqName = seqName.toUpperCase();
    }

    public String getPrefix() {
        return prefix;
    }

    public String getSeqName() {
        return seqName;
    }

}
