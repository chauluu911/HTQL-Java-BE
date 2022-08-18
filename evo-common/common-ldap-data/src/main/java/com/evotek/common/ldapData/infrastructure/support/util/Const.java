package com.evotek.common.ldapData.infrastructure.support.util;

import com.evotek.common.ldapcore.config.LDAPProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;

@EnableConfigurationProperties(LDAPProperties.class)
public class Const {
    public static final String TLS = "TLS";
    public static final String LDAPS = "ldaps";
    public static final int MAX_PAGE_SIZE_SYNC_LDAP = 100;
    public static final String COMMON_NAME = "CN";
    public static final String OBJECT_CLASS_TOP = "top";
    public static final String OBJECT_CLASS_GROUP = "group";
    public static final String OBJECT_CLASS_USER = "user";
    public static final String OBJECT_CLASS_PERSON = "person";

    public static final String OBJECT_CLASS_ORGANIZATIONAL_PERSON = "organizationalPerson";


    private Const() {
        throw new IllegalStateException("Utility class");
    }
}
