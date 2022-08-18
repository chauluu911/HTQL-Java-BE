package com.evotek.common.ldapcore.config;

import lombok.Data;
import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Data
@Component
@ConfigurationProperties(prefix = "spring.ldap")
public class LDAPProperties {
    private String urls;
    private String username;
    private String password;
    private String prefixMail;
    private String base;
    private String domain;
    private FilteredGroup filteredGroup;

    @Getter
    @Setter
    public static class FilteredGroup {

        private String baseDnGroup;

        private String[] groups;

        private String prefix;

        private Boolean enabled;
    }
}
