package com.evotek.iam.application.service.impl;

import com.evotek.common.config.security.ClientAuthentication;
import com.evotek.common.dto.response.iam.ClientToken;
import com.evotek.iam.application.config.CustomAuthenticateClientProvider;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@Primary
public class ClientAuthenticationServiceImpl implements ClientAuthentication {
    private final CustomAuthenticateClientProvider customAuthenticateClientProvider;

    public ClientAuthenticationServiceImpl(CustomAuthenticateClientProvider customAuthenticateClientProvider) {
        this.customAuthenticateClientProvider = customAuthenticateClientProvider;
    }

    @Cacheable(cacheNames = "client-token", key = "#clientId",
            condition = "#clientId != null", unless = "#clientId == null || #result == null")
    @Override
    public ClientToken getClientToken(String clientId, String clientSecret) {

        log.info("Client {} login", clientId);
        return customAuthenticateClientProvider.authenticationClient(clientId, clientSecret);
    }
}
