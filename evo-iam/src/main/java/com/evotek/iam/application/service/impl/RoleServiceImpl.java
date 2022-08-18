package com.evotek.iam.application.service.impl;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.mapper.util.PageableMapperUtil;
import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.iam.application.dto.request.RoleAutocompleteRequest;
import com.evotek.iam.application.dto.request.RoleCreateRequest;
import com.evotek.iam.application.dto.request.RoleSearchRequest;
import com.evotek.iam.application.dto.request.RoleUpdateRequest;
import com.evotek.iam.application.mapper.AutoMapper;
import com.evotek.iam.application.service.RoleService;
import com.evotek.iam.domain.Permission;
import com.evotek.iam.domain.Role;
import com.evotek.iam.domain.command.RoleCreateOrUpdateCmd;
import com.evotek.iam.domain.respository.RoleDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.PermissionEntity;
import com.evotek.iam.infrastructure.persistence.entity.RoleEntity;
import com.evotek.iam.infrastructure.persistence.mapper.PermissionEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.RoleEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.PermissionEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.RoleEntityRepository;
import com.evotek.iam.infrastructure.support.enums.RoleStatus;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

@Service
@Slf4j
public class RoleServiceImpl implements RoleService {

    private final RoleDomainRepository roleDomainRepository;
    private final RoleEntityRepository roleEntityRepository;
    private final PermissionEntityRepository permissionEntityRepository;
    private final RoleEntityMapper roleEntityMapper;
    private final PermissionEntityMapper permissionEntityMapper;
    private final AutoMapper autoMapper;

    public RoleServiceImpl(RoleDomainRepository roleDomainRepository, RoleEntityRepository roleEntityRepository,
                           PermissionEntityRepository permissionEntityRepository,
                           RoleEntityMapper roleEntityMapper,
                           PermissionEntityMapper permissionEntityMapper,
                           AutoMapper autoMapper) {
        this.roleDomainRepository = roleDomainRepository;
        this.roleEntityRepository = roleEntityRepository;
        this.permissionEntityRepository = permissionEntityRepository;
        this.roleEntityMapper = roleEntityMapper;
        this.permissionEntityMapper = permissionEntityMapper;
        this.autoMapper = autoMapper;
    }

    @Override
    public PageDTO<Role> search(RoleSearchRequest request) {

        Pageable pageable = PageableMapperUtil.toPageable(request);

        Page<RoleEntity> roleEntityPage
                = this.roleEntityRepository.search(SqlUtils.encodeKeyword(request.getKeyword()), pageable);
        List<Role> roles = this.roleEntityMapper.toDomain(roleEntityPage.toList());

        return PageDTO.of(roles, request.getPageIndex(), request.getPageSize(), roleEntityPage.getTotalElements());
    }

    @Override
    @Transactional
    public Role create(RoleCreateRequest request) {
        List<RoleEntity> roleEntitiesByCode = roleEntityRepository.findAllByCodes(List.of(request.getCode()));
        if (!CollectionUtils.isEmpty(roleEntitiesByCode)) {
            throw new ResponseException(BadRequestError.ROLE_CODE_EXISTED);
        }
        List<RoleEntity> roleEntitiesByRoot = this.roleEntityRepository.findAllRootRole();
        if (!CollectionUtils.isEmpty(roleEntitiesByRoot) && Boolean.TRUE.equals(request.getIsRoot())) {
            throw new ResponseException(BadRequestError.ADMIN_ROLE_ALREADY_EXISTED);
        }
        RoleCreateOrUpdateCmd cmd = this.autoMapper.from(request);
        List<Permission> permissions = existedPermissions();
        Role role = new Role(cmd, permissions);
        this.roleDomainRepository.save(role);
        return role;
    }

    @Override
    @Transactional
    public Role update(String roleId, RoleUpdateRequest request) {
        List<RoleEntity> roleEntities = this.roleEntityRepository.findAllRootRole();
        if (!CollectionUtils.isEmpty(roleEntities) && Boolean.TRUE.equals(request.getIsRoot())) {
            throw new ResponseException(BadRequestError.ADMIN_ROLE_ALREADY_EXISTED);
        }
        Role role = this.roleDomainRepository.getById(roleId);
        RoleCreateOrUpdateCmd cmd = this.autoMapper.from(request);
        List<Permission> permissions = existedPermissions();
        role.update(cmd, permissions);
        this.roleDomainRepository.save(role);
        return role;
    }

    @Override
    public Role getById(String id) {
        return this.roleDomainRepository.getById(id);
    }

    @Override
    @Transactional
    public void delete(String roleId) {
        Role role = this.roleDomainRepository.getById(roleId);
        role.deleted();
        this.roleDomainRepository.save(role);
    }

    @Override
    public PageDTO<Role> searchByRoleRequest(RoleSearchRequest request) {
        List<RoleEntity> roleEntities = this.roleEntityRepository.search(request);
        List<Role> roles = this.roleEntityMapper.toDomain(roleEntities);
        Long count = this.roleEntityRepository.count(request);

        return PageDTO.of(roles, request.getPageIndex(), request.getPageSize(), count);
    }

    @Override
    public PageDTO<Role> autocomplete(RoleAutocompleteRequest request) {
        Pageable pageable = PageableMapperUtil.toPageable(request);

        Page<RoleEntity> roleEntityPage = this.roleEntityRepository
                .search(SqlUtils.encodeKeyword(request.getKeyword()), RoleStatus.ACTIVE, pageable);
        List<Role> roles = this.roleEntityMapper.toDomain(roleEntityPage.toList());
        return PageDTO.of(roles, request.getPageIndex(), request.getPageSize(), roleEntityPage.getTotalElements());
    }

    @Override
    @Transactional
    public void active(String id) {
        Role role = this.roleDomainRepository.getById(id);
        role.active();
        this.roleDomainRepository.save(role);
    }

    @Override
    @Transactional
    public void inactive(String id) {
        Role role = this.roleDomainRepository.getById(id);
        role.inactive();
        this.roleDomainRepository.save(role);
    }

    private List<Permission> existedPermissions() {
        List<PermissionEntity> permissionEntities = this.permissionEntityRepository.findAllActivated();
        if (permissionEntities == null) {
            return new ArrayList<>();
        }
        return this.permissionEntityMapper.toDomain(permissionEntities);
    }
}
