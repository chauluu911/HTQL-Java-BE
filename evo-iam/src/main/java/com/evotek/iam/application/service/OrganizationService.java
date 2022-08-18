package com.evotek.iam.application.service;

import com.evotek.common.dto.PageDTO;
import com.evotek.iam.application.dto.request.*;
import com.evotek.iam.domain.Organization;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

public interface OrganizationService {

    Organization create(OrganizationCreateRequest request);

    Organization update(String id, OrganizationUpdateRequest request);

    void delete(String id);

    Organization ensureExisted(String uuid);

    void active(String id);

    PageDTO<Organization> search(OrganizationSearchRequest request);

    Organization getById(String id);

    List<Organization> findByIds(OrganizationGetByIdsRequest request);

    Optional<Organization> findOrganizationByBusinessCode(String businessCode);

    Optional<Organization> findOrganizationEntityByEmail(String email);

    void inactive(String id);
}

