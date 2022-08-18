package com.evotek.common.config.security.impl;

import com.evotek.common.client.iam.IAMClient;
import com.evotek.common.config.security.ClientAuthentication;
import com.evotek.common.dto.request.iam.ClientLoginRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.iam.ClientToken;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

@Service
public class RemoteClientService implements ClientAuthentication {

    private final IAMClient iamClient;

    public RemoteClientService(@Lazy IAMClient iamClient) {
        this.iamClient = iamClient;
    }

    @Cacheable(cacheNames = "client-token", key = "#clientId",
            condition = "#clientId != null", unless = "#clientId == null || #result == null")
    @Override
    public ClientToken getClientToken(String clientId, String clientSecret) {
        Response<ClientToken> clientTokenResponse = iamClient.getTokenClient(new ClientLoginRequest(clientId, clientSecret));
        if (clientTokenResponse.isSuccess() && clientTokenResponse.getData() != null) {
            return clientTokenResponse.getData();
        }
        return null;
    }
}
