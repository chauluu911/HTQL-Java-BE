package com.evotek.common.config;

import com.evotek.common.config.security.ClientAuthentication;
import com.evotek.common.dto.response.iam.ClientToken;
import feign.RequestInterceptor;
import feign.RequestTemplate;
import org.springframework.beans.factory.annotation.Value;

import java.util.Collection;
import java.util.List;
import java.util.Map;

public class FeignClientInterceptor implements RequestInterceptor {

    private static final String AUTHORIZATION_HEADER = "Authorization";
    private static final String TOKEN_TYPE = "Bearer";
    private static final String URI_CLIENT_AUTHENTICATE = "/api/client/authenticate";

    private ClientAuthentication clientAuthentication;

    @Value("${app.iam.client.client-id}")
    private String clientId;

    @Value("${app.iam.client.client-secret}")
    private String clientSecret;

    public FeignClientInterceptor() {
    }

    public ClientAuthentication getClientAuthentication() {
        return clientAuthentication;
    }

    public void setClientAuthentication(ClientAuthentication clientAuthentication) {
        this.clientAuthentication = clientAuthentication;
    }

    @Override
    public void apply(RequestTemplate requestTemplate) {
        // if request to api get client token then skip
        if (requestTemplate.url().contains(URI_CLIENT_AUTHENTICATE)) {
            return;
        }

        // if token client was in headers then forward request
        // don't need, run once time
        Map<String, Collection<String>> headers = requestTemplate.headers();
        List<String> authorization = (List<String>) headers.get(AUTHORIZATION_HEADER);
        if (authorization != null && authorization.get(0).startsWith(TOKEN_TYPE)) {
            return;
        }

        ClientToken clientTokenResponse = clientAuthentication.getClientToken(clientId, clientSecret);
        if (clientTokenResponse != null) {
            requestTemplate.header(AUTHORIZATION_HEADER, String.format("%s %s", TOKEN_TYPE, clientTokenResponse.getToken()));
        }
    }
}
