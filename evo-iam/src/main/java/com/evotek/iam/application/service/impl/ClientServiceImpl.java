package com.evotek.iam.application.service.impl;

import com.evotek.common.UserAuthority;
import com.evotek.common.dto.PageDTO;
import com.evotek.common.error.AuthenticationError;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.security.AuthorityService;
import com.evotek.common.webapp.support.SecurityUtils;
import com.evotek.iam.application.dto.request.ClientCreateRequest;
import com.evotek.iam.application.dto.request.ClientSearchRequest;
import com.evotek.iam.application.dto.request.ClientUpdateRequest;
import com.evotek.iam.application.dto.response.ClientResponse;
import com.evotek.iam.application.mapper.AutoMapper;
import com.evotek.iam.application.mapper.AutoMapperQuery;
import com.evotek.iam.application.service.ClientService;
import com.evotek.iam.domain.Client;
import com.evotek.iam.domain.command.ClientCreateCmd;
import com.evotek.iam.domain.command.ClientUpdateCmd;
import com.evotek.iam.domain.query.ClientSearchQuery;
import com.evotek.iam.domain.respository.ClientDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.ClientEntity;
import com.evotek.iam.infrastructure.persistence.mapper.ClientEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.ClientEntityRepository;
import com.evotek.iam.infrastructure.support.enums.ClientStatus;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Caching;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

@Service
@Slf4j
public class ClientServiceImpl implements ClientService {

    private final ClientEntityRepository clientEntityRepository;
    private final ClientDomainRepository clientDomainRepository;
    private final AutoMapper autoMapper;
    private final AutoMapperQuery autoMapperQuery;
    private final ClientEntityMapper clientEntityMapper;
    private final AuthorityService authorityService;

    public ClientServiceImpl(ClientEntityRepository clientEntityRepository,
                             ClientDomainRepository clientDomainRepository,
                             AutoMapper autoMapper,
                             AutoMapperQuery autoMapperQuery,
                             ClientEntityMapper clientEntityMapper, AuthorityService authorityService) {
        this.clientEntityRepository = clientEntityRepository;
        this.clientDomainRepository = clientDomainRepository;
        this.autoMapper = autoMapper;
        this.autoMapperQuery = autoMapperQuery;
        this.clientEntityMapper = clientEntityMapper;
        this.authorityService = authorityService;
    }

    @Override
    @Transactional
    public ClientResponse create(ClientCreateRequest request) {
        Optional<ClientEntity> optionalClientEntity
                = this.clientEntityRepository.findByClientName(request.getName().toLowerCase());
        if (optionalClientEntity.isPresent()) {
            throw new ResponseException(BadRequestError.CLIENT_NAME_EXITED);
        }
        ClientCreateCmd cmd = this.autoMapper.from(request);
        Client client = new Client(cmd);
        this.clientDomainRepository.save(client);
        ClientResponse clientResponse = this.autoMapper.toClientResponse(client);
        clientResponse.setSecretToken(client.getSecret());
        return clientResponse;
    }

    @Override
    public Client update(String id, ClientUpdateRequest request) {
        Client client = this.clientDomainRepository.getById(id);
        if (!Objects.equals(client.getName(), request.getName())) {
            Optional<ClientEntity> optionalClientEntity
                    = this.clientEntityRepository.findByClientName(request.getName().toLowerCase());
            if (optionalClientEntity.isPresent()) {
                throw new ResponseException(BadRequestError.CLIENT_NAME_EXITED);
            }
        }
        ClientUpdateCmd cmd = this.autoMapper.from(request);
        client.update(cmd);
        return this.clientDomainRepository.save(client);
    }

    @Override
    public PageDTO<Client> searchClient(ClientSearchRequest request) {
        ClientSearchQuery query = this.autoMapperQuery.toQuery(request);
        Long total = this.clientEntityRepository.count(query);
        if (Objects.equals(total, 0L)) {
            return PageDTO.empty();
        }
        List<Client> clients = this.clientEntityMapper.toDomain(this.clientEntityRepository.searchClient(query));
        return new PageDTO<>(clients, request.getPageIndex(), request.getPageSize(), total);
    }

    @Override
    public Client getById(String id) {
        return this.clientDomainRepository.getById(id);
    }

    @Override
    @Caching(evict = {
            @CacheEvict(value = "client-authority", key = "#id"),
            @CacheEvict(value = "client-token", key = "#id")})
    public Client changeStatus(String id, ClientStatus status) {
        Client client = this.clientDomainRepository.getById(id);
        client.changeStatus(status);
        return this.clientDomainRepository.save(client);
    }

    @Override
    public UserAuthority getMyAuthority() {
        String clientId = SecurityUtils.getCurrentUserLoginId()
                .orElseThrow(() -> new ResponseException(AuthenticationError.UNAUTHORISED));
        return authorityService.getClientAuthority(clientId);
    }
}
