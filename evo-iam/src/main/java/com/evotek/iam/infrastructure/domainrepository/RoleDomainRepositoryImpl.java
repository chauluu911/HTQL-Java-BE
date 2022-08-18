package com.evotek.iam.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.iam.domain.Role;
import com.evotek.iam.domain.respository.RoleDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.RoleEntity;
import com.evotek.iam.infrastructure.persistence.entity.RolePermissionEntity;
import com.evotek.iam.infrastructure.persistence.mapper.RoleEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.RolePermissionEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.RoleEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.RolePermissionEntityRepository;
import com.evotek.iam.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Service
@Slf4j
public class RoleDomainRepositoryImpl extends AbstractDomainRepository<Role, RoleEntity, String> implements RoleDomainRepository {

    private final RoleEntityRepository roleEntityRepository;
    private final RolePermissionEntityRepository rolePermissionEntityRepository;
    private final RoleEntityMapper roleEntityMapper;
    private final RolePermissionEntityMapper rolePermissionEntityMapper;

    public RoleDomainRepositoryImpl(RoleEntityRepository roleEntityRepository,
                                    RolePermissionEntityRepository rolePermissionEntityRepository,
                                    RoleEntityMapper roleEntityMapper,
                                    RolePermissionEntityMapper rolePermissionEntityMapper) {
        super(roleEntityRepository, roleEntityMapper);
        this.roleEntityRepository = roleEntityRepository;
        this.rolePermissionEntityRepository = rolePermissionEntityRepository;
        this.roleEntityMapper = roleEntityMapper;
        this.rolePermissionEntityMapper = rolePermissionEntityMapper;
    }

    @Override
    @Transactional
    public Role save(Role role) {
        RoleEntity roleEntity = this.roleEntityMapper.toEntity(role);
        this.roleEntityRepository.save(roleEntity);
        if (!CollectionUtils.isEmpty(role.getPermissions())) {
            List<RolePermissionEntity> rolePermissionEntities
                    = this.rolePermissionEntityMapper.toEntity(role.getPermissions());
            this.rolePermissionEntityRepository.saveAll(rolePermissionEntities);
        }

        return this.roleEntityMapper.toDomain(roleEntity);
    }

    @Override
    protected Role enrich(Role role) {
        List<RolePermissionEntity> rolePermissionEntities = this.rolePermissionEntityRepository.findAllByRoleId(role.getId());
        if (Objects.isNull(rolePermissionEntities)) {
            role.enrichPermissions(new ArrayList<>());
        } else {
            role.enrichPermissions(this.rolePermissionEntityMapper.toDomain(rolePermissionEntities));
        }
        return role;
    }

    @Override
    public Role getById(String id) {
        return this.findById(id).orElseThrow(() ->
                new ResponseException(NotFoundError.ROLE_NOT_FOUND));
    }
}
