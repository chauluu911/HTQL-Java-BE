package com.evotek.common.config.security;

import com.evotek.common.dto.response.iam.ClientToken;

public interface ClientAuthentication {

    ClientToken getClientToken(String clientId, String clientSecret);
}
