package com.evotek.common.webapp.security.impl;

import com.evotek.common.UserAuthority;
import com.evotek.common.client.iam.IAMClient;
import com.evotek.common.dto.response.Response;
import com.evotek.common.webapp.security.AuthorityService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
public class RemoteAuthorityService implements AuthorityService {

    private final IAMClient iamClient;

    @Autowired
    public RemoteAuthorityService(IAMClient iamClient) {
        this.iamClient = iamClient;
    }

    @Cacheable(cacheNames = "user-authority", key = "#userId",
            condition = "#userId != null", unless = "#userId == null || #result == null")
    @Override
    public UserAuthority getUserAuthority(String userId) {
        Response<UserAuthority> response = iamClient.getUserAuthority(userId);
        if (response.isSuccess() && Objects.nonNull(response.getData())) {
            return response.getData();
        }
        return null;
    }

    @Cacheable(cacheNames = "client-authority", key = "#clientId",
            condition = "#clientId != null", unless = "#clientId == null || #result == null")
    @Override
    public UserAuthority getClientAuthority(String clientId) {
        Response<UserAuthority> response = iamClient.getClientAuthority();
        if (response.isSuccess() && Objects.nonNull(response.getData())) {
            return response.getData();
        }
        return null;
    }
}
