package com.evotek.iam.application.service.impl;

import com.evotek.common.webapp.i18n.LocaleStringService;
import com.evotek.common.webapp.support.AbstractDomainService;
import com.evotek.iam.application.service.PermissionService;
import com.evotek.iam.domain.Permission;
import com.evotek.iam.infrastructure.persistence.entity.PermissionEntity;
import com.evotek.iam.infrastructure.persistence.mapper.PermissionEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.PermissionEntityRepository;
import com.evotek.iam.infrastructure.support.enums.ResourceCategory;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Slf4j
public class PermissionServiceImpl extends AbstractDomainService<Permission, PermissionEntity, String> implements PermissionService {

    private final PermissionEntityRepository permissionEntityRepository;
    private final PermissionEntityMapper permissionEntityMapper;
    private final LocaleStringService localeStringService;

    public PermissionServiceImpl(PermissionEntityRepository permissionEntityRepository,
                                 PermissionEntityMapper permissionEntityMapper,
                                 LocaleStringService localeStringService) {
        super(permissionEntityRepository, permissionEntityMapper);
        this.permissionEntityRepository = permissionEntityRepository;
        this.permissionEntityMapper = permissionEntityMapper;
        this.localeStringService = localeStringService;
    }

    /**
     * find all Permission
     *
     * @return list permission
     */
    @Override
    public List<Permission> findAll() {
        List<PermissionEntity> permissionEntities = this.permissionEntityRepository.findAllActivated();
        ResourceCategory[] resourceCategories = ResourceCategory.values();
        List<Permission> permissions = this.permissionEntityMapper.toDomain(permissionEntities);
        permissions.forEach(p -> {
            for (ResourceCategory perType : resourceCategories) {
                if (p.getResourceCode().equals(perType.getResourceCode())) {
                    p.enrichResourceName(this.localeStringService.getMessage(perType.getResourceName(), ""));
                }
            }
        });
        return permissions;
    }

}