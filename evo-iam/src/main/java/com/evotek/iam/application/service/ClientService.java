package com.evotek.iam.application.service;

import com.evotek.common.UserAuthority;
import com.evotek.common.dto.PageDTO;
import com.evotek.iam.application.dto.request.ClientCreateRequest;
import com.evotek.iam.application.dto.request.ClientSearchRequest;
import com.evotek.iam.application.dto.request.ClientUpdateRequest;
import com.evotek.iam.application.dto.response.ClientResponse;
import com.evotek.iam.domain.Client;
import com.evotek.iam.infrastructure.support.enums.ClientStatus;

public interface ClientService {

    ClientResponse create(ClientCreateRequest request);

    Client update(String id, ClientUpdateRequest request);

    PageDTO<Client> searchClient(ClientSearchRequest request);

    Client getById(String id);

    Client changeStatus(String id, ClientStatus status);

    UserAuthority getMyAuthority();
}

