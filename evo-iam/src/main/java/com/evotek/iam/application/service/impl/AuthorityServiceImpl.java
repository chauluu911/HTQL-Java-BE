package com.evotek.iam.application.service.impl;

import com.evotek.common.UserAuthority;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.security.AuthorityService;
import com.evotek.iam.domain.Client;
import com.evotek.iam.domain.Role;
import com.evotek.iam.domain.respository.ClientDomainRepository;
import com.evotek.iam.domain.respository.RoleDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.RoleEntity;
import com.evotek.iam.infrastructure.persistence.entity.RolePermissionEntity;
import com.evotek.iam.infrastructure.persistence.entity.UserEntity;
import com.evotek.iam.infrastructure.persistence.entity.UserRoleEntity;
import com.evotek.iam.infrastructure.persistence.repository.RoleEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.RolePermissionEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.UserEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.UserRoleEntityRepository;
import com.evotek.iam.infrastructure.support.enums.RoleStatus;
import com.evotek.iam.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@Primary
@Slf4j
public class AuthorityServiceImpl implements AuthorityService {

    private final UserRoleEntityRepository userRoleEntityRepository;
    private final RoleEntityRepository roleEntityRepository;
    private final RolePermissionEntityRepository rolePermissionEntityRepository;
    private final UserEntityRepository userEntityRepository;
    private final RoleDomainRepository roleDomainRepository;
    private final ClientDomainRepository clientDomainRepository;

    public AuthorityServiceImpl(UserRoleEntityRepository userRoleEntityRepository,
                                RoleEntityRepository roleEntityRepository,
                                RolePermissionEntityRepository rolePermissionEntityRepository,
                                UserEntityRepository userEntityRepository, RoleDomainRepository roleDomainRepository, ClientDomainRepository clientDomainRepository) {
        this.userRoleEntityRepository = userRoleEntityRepository;
        this.roleEntityRepository = roleEntityRepository;
        this.rolePermissionEntityRepository = rolePermissionEntityRepository;
        this.userEntityRepository = userEntityRepository;
        this.roleDomainRepository = roleDomainRepository;
        this.clientDomainRepository = clientDomainRepository;
    }

    @Cacheable(cacheNames = "user-authority", key = "#userId",
            condition = "#userId != null", unless = "#userId == null || #result == null")
    @Override
    public UserAuthority getUserAuthority(String userId) {
        UserEntity userEntity = ensureUserExisted(userId);
        List<String> grantedAuthorities = new ArrayList<>();
        boolean isRoot = false;
        List<UserRoleEntity> userRoleEntities = this.userRoleEntityRepository.findAllByUserId(userEntity.getId());
        if (!CollectionUtils.isEmpty(userRoleEntities)) {
            List<String> roleIds = userRoleEntities.stream()
                    .map(UserRoleEntity::getRoleId).distinct().collect(Collectors.toList());
            List<RoleEntity> roleEntities = this.roleEntityRepository.findAllByIds(roleIds);
            roleIds = roleEntities.stream()
                    .filter(r -> Objects.equals(RoleStatus.ACTIVE, r.getStatus()))
                    .map(RoleEntity::getId).distinct().collect(Collectors.toList());
            isRoot = roleEntities.stream().anyMatch(r -> Boolean.TRUE.equals(r.getIsRoot()));
            List<RolePermissionEntity> rolePermissionEntities = this.rolePermissionEntityRepository.findAllByRoleIds(roleIds);
            if (!CollectionUtils.isEmpty(rolePermissionEntities)) {
                grantedAuthorities = rolePermissionEntities.stream()
                        .map(r -> String.format("%s:%s", r.getResourceCode().toLowerCase(), r.getScope().toString().toLowerCase()))
                        .distinct().collect(Collectors.toList());
            }
        } else {
            log.info("User {} don't has role", userId);
        }

        return UserAuthority.builder()
                .isRoot(isRoot)
                .grantedPermissions(grantedAuthorities)
                .userId(userEntity.getId())
                .accountType(userEntity.getAccountType())
                .lastAuthChangeAt(userEntity.getLastAuthChangeAt())
                .build();
    }

    @Override
    public UserAuthority getClientAuthority(String clientId) {
        List<String> grantedAuthorities = new ArrayList<>();
        Client client = this.clientDomainRepository.getById(clientId);
        boolean isRoot = false;
        if (Objects.nonNull(client.getRoleId())) {
            Role role = this.roleDomainRepository.getById(client.getRoleId());
            isRoot = role.getIsRoot();
            if (!CollectionUtils.isEmpty(role.getPermissions())) {
                grantedAuthorities = role.getPermissions().stream()
                        .map(r -> String.format("%s:%s", r.getResourceCode().toLowerCase(), r.getScope().toString().toLowerCase()))
                        .distinct().collect(Collectors.toList());
            }
        } else {
            log.info("Client {} don't has role", client);
        }
        return UserAuthority.builder()
                .isRoot(isRoot)
                .grantedPermissions(grantedAuthorities)
                .build();
    }

    private UserEntity ensureUserExisted(String userId) {
        return this.userEntityRepository.findById(userId).orElseThrow(() ->
                new ResponseException(NotFoundError.USER_NOT_FOUND));
    }
}
