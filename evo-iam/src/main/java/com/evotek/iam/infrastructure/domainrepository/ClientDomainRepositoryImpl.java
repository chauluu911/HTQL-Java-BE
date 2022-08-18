package com.evotek.iam.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.iam.domain.Client;
import com.evotek.iam.domain.respository.ClientDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.ClientEntity;
import com.evotek.iam.infrastructure.persistence.mapper.ClientEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.ClientEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.RoleEntityRepository;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class ClientDomainRepositoryImpl extends AbstractDomainRepository<Client, ClientEntity, String> implements ClientDomainRepository {
    private final RoleEntityRepository roleEntityRepository;

    public ClientDomainRepositoryImpl(ClientEntityRepository clientEntityRepository,
                                      ClientEntityMapper clientEntityMapper, RoleEntityRepository roleEntityRepository) {
        super(clientEntityRepository, clientEntityMapper);
        this.roleEntityRepository = roleEntityRepository;
    }

    @Override
    public Client getById(String id) {
        return this.findById(id).orElseThrow(() -> new ResponseException(BadRequestError.CLIENT_NOT_EXISTED));
    }
}

