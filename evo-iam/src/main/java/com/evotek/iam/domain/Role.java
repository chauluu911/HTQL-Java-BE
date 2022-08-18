package com.evotek.iam.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.enums.Scope;
import com.evotek.common.util.IdUtils;
import com.evotek.iam.domain.command.RoleCreateOrUpdateCmd;
import com.evotek.iam.domain.command.RolePermissionCreateCmd;
import com.evotek.iam.infrastructure.support.enums.RoleStatus;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class Role extends AuditableDomain {

    private String id;
    private String code;
    private String name;
    private String description;
    private Boolean isRoot;
    private RoleStatus status;
    private List<RolePermission> permissions;
    private Boolean deleted;

    public Role(RoleCreateOrUpdateCmd cmd, List<Permission> existedPermissions) {
        this.id = IdUtils.nextId();
        this.code = cmd.getCode();
        this.name = cmd.getName();
        this.description = cmd.getDescription();
        this.status = RoleStatus.ACTIVE;


        if (Objects.isNull(cmd.getIsRoot())) {
            cmd.setIsRoot(false);
        }
        this.isRoot = cmd.getIsRoot();

        this.permissions = new ArrayList<>();
        if ((!Boolean.TRUE.equals(this.isRoot)) && (!CollectionUtils.isEmpty(cmd.getPermissions()))) {
            List<RolePermissionCreateCmd> permissionCmds = cmd.getPermissions().stream().distinct().collect(Collectors.toList());
            for (RolePermissionCreateCmd permissionCmd : permissionCmds) {
                List<Permission> permissionResources = existedPermissions.stream()
                        .filter(p -> Objects.equals(permissionCmd.getResourceCode(), p.getResourceCode()))
                        .collect(Collectors.toList());

                if (!CollectionUtils.isEmpty(permissionCmd.getScopes())) {
                    for (Scope scope : permissionCmd.getScopes()) {
                        Optional<Permission> optionalPermission = permissionResources.stream()
                                .filter(p -> scope.equals(p.getScope())).findFirst();
                        optionalPermission.ifPresent(permission
                                -> this.permissions.add(new RolePermission(this.id, permission.getResourceCode(), permission.getScope())));
                    }
                }
            }
        }
        this.deleted = false;
    }

    public void update(RoleCreateOrUpdateCmd cmd, List<Permission> existedPermissions) {
        this.name = cmd.getName();
        this.description = cmd.getDescription();
        if (Objects.isNull(cmd.getIsRoot())) {
            cmd.setIsRoot(false);
        }
        this.isRoot = cmd.getIsRoot();

        if (CollectionUtils.isEmpty(this.permissions)) {
            this.permissions = new ArrayList<>();
        }
        this.permissions.forEach(RolePermission::deleted);
        if ((!Boolean.TRUE.equals(this.isRoot)) && (!CollectionUtils.isEmpty(cmd.getPermissions()))) {
            List<RolePermissionCreateCmd> permissionCmds = cmd.getPermissions().stream().distinct().collect(Collectors.toList());
            for (RolePermissionCreateCmd permissionCmd : permissionCmds) {
                List<Permission> permissionResources = existedPermissions.stream()
                        .filter(p -> Objects.equals(permissionCmd.getResourceCode(), p.getResourceCode()))
                        .collect(Collectors.toList());

                if (!CollectionUtils.isEmpty(permissionCmd.getScopes())) {
                    for (Scope scope : permissionCmd.getScopes()) {
                        Optional<Permission> optionalPermission = permissionResources.stream()
                                .filter(p -> scope.equals(p.getScope())).findFirst();
                        optionalPermission.ifPresent(permission -> {
                            Optional<RolePermission> optionalRolePermission = this.permissions.stream()
                                    .filter(p -> Objects.equals(this.id, p.getRoleId())
                                            && Objects.equals(permission.getResourceCode(), p.getResourceCode())
                                            && Objects.equals(permission.getScope(), p.getScope())
                                    ).findFirst();
                            if (optionalRolePermission.isEmpty()) {
                                this.permissions.add(new RolePermission(this.id, permission.getResourceCode(), permission.getScope()));
                            } else {
                                optionalRolePermission.get().unDelete();
                            }
                        });
                    }
                }
            }
        }
        this.deleted = false;
    }

    public void deleted() {
        this.deleted = true;
        if (!CollectionUtils.isEmpty(this.permissions)) {
            this.permissions = new ArrayList<>();
        }
        this.permissions.forEach(RolePermission::deleted);
    }

    public void enrichPermissions(List<RolePermission> permissions) {
        this.permissions = permissions;
    }

    public void active() {
        this.status = RoleStatus.ACTIVE;
    }

    public void inactive() {
        this.status = RoleStatus.INACTIVE;
    }
}
