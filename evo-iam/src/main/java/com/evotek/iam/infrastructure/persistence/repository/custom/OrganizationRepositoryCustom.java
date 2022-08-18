package com.evotek.iam.infrastructure.persistence.repository.custom;

import com.evotek.iam.infrastructure.persistence.entity.OrganizationEntity;
import com.evotek.iam.domain.query.OrganizationSearchQuery;

import java.util.List;

public interface OrganizationRepositoryCustom {
    List<OrganizationEntity> search(OrganizationSearchQuery query);

    Long countOrganization(OrganizationSearchQuery query);
}
