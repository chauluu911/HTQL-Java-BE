package com.evotek.iam.application.config;

import com.evotek.common.dto.response.iam.ClientToken;
import com.evotek.common.error.AuthenticationError;
import com.evotek.common.exception.ResponseException;
import com.evotek.iam.infrastructure.persistence.entity.ClientEntity;
import com.evotek.iam.infrastructure.persistence.repository.ClientEntityRepository;
import com.evotek.iam.infrastructure.support.enums.ClientStatus;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class CustomAuthenticateClientProvider {

    private final ClientEntityRepository clientEntityRepository;
    private final TokenProvider tokenProvider;

    public CustomAuthenticateClientProvider(ClientEntityRepository clientEntityRepository, TokenProvider tokenProvider) {
        this.clientEntityRepository = clientEntityRepository;
        this.tokenProvider = tokenProvider;
    }

    public ClientToken authenticationClient(String clientId, String clientSecret) {
        ClientEntity clientEntity = clientEntityRepository.findByClientIdAndSecret(clientId, clientSecret, ClientStatus.ACTIVE).orElseThrow(() ->
                new ResponseException(AuthenticationError.UNAUTHORISED));
        log.info("Client {} login success", clientEntity.getId());
        String token = tokenProvider.createClientToken(clientId);
        return ClientToken.builder()
                .token(token)
                .build();
    }
}
