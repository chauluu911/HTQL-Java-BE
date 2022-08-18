package com.evotek.iam.infrastructure.persistence.repository.custom;

import com.evotek.iam.domain.query.ClientSearchQuery;
import com.evotek.iam.infrastructure.persistence.entity.ClientEntity;

import java.util.List;

public interface ClientRepositoryCustom {

    List<ClientEntity> searchClient(ClientSearchQuery query);

    Long count(ClientSearchQuery query);
}
