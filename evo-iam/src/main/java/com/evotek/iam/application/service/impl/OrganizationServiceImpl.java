package com.evotek.iam.application.service.impl;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.persistence.support.SeqRepository;
import com.evotek.iam.domain.respository.OrganizationDomainRepository;
import com.evotek.iam.application.dto.request.*;
import com.evotek.iam.application.mapper.*;
import com.evotek.iam.application.service.OrganizationService;
import com.evotek.iam.domain.Organization;
import com.evotek.iam.domain.command.*;
import com.evotek.iam.infrastructure.persistence.entity.OrganizationEntity;
import com.evotek.iam.domain.query.OrganizationSearchQuery;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import com.evotek.iam.infrastructure.support.exception.NotFoundError;
import com.evotek.iam.infrastructure.persistence.mapper.OrganizationEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.OrganizationRepository;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
@Service
@Slf4j
@RequiredArgsConstructor
public class OrganizationServiceImpl implements OrganizationService {

    private final OrganizationRepository organizationRepository;
    private final OrganizationDomainRepository organizationDomainRepository;
    private final AutoMapper autoMapper;
    private final OrganizationEntityMapper organizationEntityMapper;
    private final AutoMapperQuery autoMapperQuery;
    private final SeqRepository seqRepository;

    @Override
    public PageDTO<Organization> search(OrganizationSearchRequest request) {
        OrganizationSearchQuery query = autoMapperQuery.toQuery(request);
        List<OrganizationEntity> organizationEntities = organizationRepository.search(query);
        List<Organization> organizations = this.organizationEntityMapper.toDomain(organizationEntities);
        return PageDTO.of(organizations,
                request.getPageIndex(),
                request.getPageSize(),
                organizationRepository.countOrganization(query));
    }

    @Override
    public Organization getById(String id) {
        return ensureExisted(id);
    }

    @Override
    public List<Organization> findByIds(OrganizationGetByIdsRequest request) {
        if (CollectionUtils.isEmpty(request.getIds())) {
            return new ArrayList<>();
        }
        List<OrganizationEntity> organizationEntities = this.organizationRepository.findAllByIds(request.getIds());
        List<Organization> organizations = this.organizationEntityMapper.toDomain(organizationEntities);
        log.info("find organization by ids, total: {}", organizations.size());
        return organizations;
    }

    @Override
    @Transactional
    public Organization create(OrganizationCreateRequest request) {
        Optional<Organization> organizationEntity = findOrganizationByBusinessCode(request.getBusinessCode());
        if (organizationEntity.isPresent()) {
            throw new ResponseException(BadRequestError.BUSINESS_CODE_EXISTED);
        }
        Optional<Organization> organizationEntityByEmail = findOrganizationEntityByEmail(request.getEmail());
        if (organizationEntityByEmail.isPresent()) {
            throw new ResponseException(BadRequestError.ORGANIZATION_EMAIL_EXITED);
        }
        OrganizationCreateCmd cmd = this.autoMapper.from(request);
        Organization organization = new Organization(cmd);
        organization.enrichCode(seqRepository.generateOrganizationCode());
        this.organizationDomainRepository.save(organization);
        return organization;
    }

    @Override
    @Transactional
    public Organization update(String id, OrganizationUpdateRequest request) {
        Organization organization = ensureExisted(id);
        if (!Objects.equals(request.getEmail(), organization.getEmail())) {
            Optional<Organization> organizationEntityByEmail = findOrganizationEntityByEmail(request.getEmail().trim());
            if (organizationEntityByEmail.isPresent()) {
                throw new ResponseException(BadRequestError.ORGANIZATION_EMAIL_EXITED);
            }
        }
        OrganizationUpdateCmd cmd = this.autoMapper.from(request);
        organization.update(cmd);
        this.organizationDomainRepository.save(organization);
        return organization;
    }

    @Override
    @Transactional
    public void delete(String id) {
        Organization organization = this.organizationDomainRepository.getById(id);
        organization.deleted();
        this.organizationDomainRepository.save(organization);
    }

    @Override
    public Organization ensureExisted(String id) {
        OrganizationEntity organizationEntity = ensureEntityExisted(id);
        return this.organizationEntityMapper.toDomain(organizationEntity);
    }

    public OrganizationEntity ensureEntityExisted(String id) {
        return this.organizationRepository.findById(id)
                .orElseThrow(() -> new ResponseException(NotFoundError.ORGANIZATION_NOT_EXITED));
    }

    @Override
    @Transactional
    public void active(String id) {
        Organization organization = ensureExisted(id);
        organization.active();
        this.organizationDomainRepository.save(organization);
    }

    @Override
    @Transactional
    public void inactive(String id) {
        Organization organization = ensureExisted(id);
        organization.inactive();
        this.organizationDomainRepository.save(organization);
    }

    @Override
    public Optional<Organization> findOrganizationByBusinessCode(String businessCode) {
        if (!StringUtils.hasLength(businessCode)) {
            return Optional.empty();
        }
        Optional<OrganizationEntity> organizationEntity = this.organizationRepository.findByBusinessCode(businessCode);
        return organizationEntity.map(this.organizationEntityMapper::toDomain);
    }

    @Override
    public  Optional<Organization> findOrganizationEntityByEmail(String email){
        if(!StringUtils.hasLength(email)){
            return Optional.empty();
        }
        Optional<OrganizationEntity> organizationEntity = this.organizationRepository.findByEmail(email);
        return organizationEntity.map(this.organizationEntityMapper::toDomain);
    }
}
