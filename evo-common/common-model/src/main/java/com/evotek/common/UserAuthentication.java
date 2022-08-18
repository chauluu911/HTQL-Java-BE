package com.evotek.common;

import com.evotek.common.enums.AccountType;
import com.evotek.common.enums.UserLevel;
import lombok.EqualsAndHashCode;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;


@EqualsAndHashCode(callSuper = false)
public class UserAuthentication extends UsernamePasswordAuthenticationToken {

    private final boolean isRoot;

    private final boolean isClient;

    private final String userId;

    private final UserLevel userLevel;

    private final AccountType accountType;

    private final String token;

    private final List<String> grantedPermissions;

    public UserAuthentication(Object principal, Object credentials,
                              Collection<? extends GrantedAuthority> authorities,
                              Boolean isRoot,
                              Boolean isClient,
                              String userId,
                              UserLevel userLevel,
                              AccountType accountType,
                              String token) {
        super(principal, credentials, authorities);
        this.isRoot = isRoot != null && isRoot;
        this.isClient = isClient != null && isClient;
        this.userId = userId;
        this.userLevel = userLevel;
        this.accountType = accountType;
        this.token = token;
        this.grantedPermissions = CollectionUtils.isEmpty(authorities) ? new ArrayList<>()
                : authorities.stream().map(GrantedAuthority::getAuthority).collect(Collectors.toList());
    }

    public boolean isRoot() {
        return isRoot;
    }

    public boolean isClient() {
        return isClient;
    }

    public String getUserId() {
        return this.userId;
    }

    public String getToken() {
        return this.token;
    }

    public List<String> getGrantedPermissions() {
        if (CollectionUtils.isEmpty(this.grantedPermissions)) {
            return new ArrayList<>();
        }
        return grantedPermissions;
    }

    public UserLevel getUserLevel() {
        return userLevel;
    }

    public AccountType getAccountType() {
        return accountType;
    }
}
