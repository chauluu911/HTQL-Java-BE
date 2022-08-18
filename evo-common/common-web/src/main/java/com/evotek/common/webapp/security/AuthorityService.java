package com.evotek.common.webapp.security;

import com.evotek.common.UserAuthority;

public interface AuthorityService {

    UserAuthority getUserAuthority(String userId);

    UserAuthority getClientAuthority(String clientId);
}
