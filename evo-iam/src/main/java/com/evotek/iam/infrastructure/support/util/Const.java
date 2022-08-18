package com.evotek.iam.infrastructure.support.util;

public class Const {

    public static final String DEFAULT_LINK_RESET_PASSWORD = "mbevo://reset-password";
    public static final String DEFAULT_DOMAIN = "https://apiqltn.mbevo.com.vn";
    public static final String REDIRECT_LINK_CHANGE_PASSWORD = "/iam/api/account/reset-password/redirect";
    public static final int REDIRECTION_CODE = 302;
    public static final String TLS = "TLS";
    public static final String LDAPS = "ldaps";


    private Const() {
        throw new IllegalStateException("Utility class");
    }
}
