package com.evotek.iam.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.mapper.EntityMapper;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.iam.domain.Organization;
import com.evotek.iam.domain.respository.OrganizationDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.OrganizationEntity;
import com.evotek.iam.infrastructure.persistence.mapper.OrganizationEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.OrganizationRepository;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class OrganizationDomainRepositoryImpl extends AbstractDomainRepository<Organization, OrganizationEntity, String> implements OrganizationDomainRepository {
    protected OrganizationDomainRepositoryImpl(OrganizationEntityMapper organizationEntityMapper, OrganizationRepository organizationRepository) {
        super(organizationRepository, organizationEntityMapper);
    }

    @Override
    public Organization getById(String id) {
        return this.findById(id).orElseThrow(() -> new ResponseException(BadRequestError.DEPARTMENT_NOT_EXITED));
    }
}
