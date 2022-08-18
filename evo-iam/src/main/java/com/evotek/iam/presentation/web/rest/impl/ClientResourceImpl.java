package com.evotek.iam.presentation.web.rest.impl;

import com.evotek.common.UserAuthority;
import com.evotek.common.config.security.ClientAuthentication;
import com.evotek.common.dto.request.iam.ClientLoginRequest;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.iam.ClientToken;
import com.evotek.iam.application.dto.request.ClientCreateRequest;
import com.evotek.iam.application.dto.request.ClientSearchRequest;
import com.evotek.iam.application.dto.request.ClientUpdateRequest;
import com.evotek.iam.application.dto.response.ClientResponse;
import com.evotek.iam.application.service.ClientService;
import com.evotek.iam.domain.Client;
import com.evotek.iam.infrastructure.support.enums.ClientStatus;
import com.evotek.iam.presentation.web.rest.ClientResource;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class ClientResourceImpl implements ClientResource {

    private final ClientAuthentication clientAuthentication;
    private final ClientService clientService;

    public ClientResourceImpl(ClientAuthentication clientAuthentication,
                              ClientService clientService) {
        this.clientAuthentication = clientAuthentication;
        this.clientService = clientService;
    }

    @Override
    public Response<ClientToken> clientAuthentication(ClientLoginRequest request) {
        return Response.of(clientAuthentication.getClientToken(request.getClientId(), request.getClientSecret()));
    }

    @Override
    public Response<ClientResponse> createClient(ClientCreateRequest request) {
        return Response.of(clientService.create(request));
    }

    @Override
    public Response<Client> updateClient(String id, ClientUpdateRequest request) {
        return Response.of(clientService.update(id, request));
    }

    @Override
    public PagingResponse<Client> search(ClientSearchRequest request) {
        return PagingResponse.of(clientService.searchClient(request));
    }

    @Override
    public Response<Client> getById(String id) {
        return Response.of(this.clientService.getById(id));
    }

    @Override
    public Response<Client> active(String id) {
        return Response.of(this.clientService.changeStatus(id, ClientStatus.ACTIVE));
    }

    @Override
    public Response<Client> inactive(String id) {
        return Response.of(this.clientService.changeStatus(id, ClientStatus.INACTIVE));
    }

    @Override
    public Response<UserAuthority> getAuthoritiesByClientId() {
        return Response.of(clientService.getMyAuthority());
    }
}
