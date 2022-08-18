package com.evotek.iam.domain;

import com.evotek.common.enums.Scope;
import com.evotek.common.util.IdUtils;
import com.evotek.iam.domain.command.RoleCreateOrUpdateCmd;
import com.evotek.iam.domain.command.RolePermissionCreateCmd;
import com.evotek.iam.infrastructure.support.enums.RoleStatus;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class RoleTest {

    private static final String roleId = IdUtils.nextId();
    private static final String resourceCode = "testResouceCode";
    private static final String name = "testName";
    private static final String resourceName = "testResourceName";
    private static final String code = "testCode";
    private static final String description = "testDescription";
    private static final List<String> resourceCodes = new ArrayList<>();

    @BeforeAll
    private static void init() {
        resourceCodes.add("Product screen");
        resourceCodes.add("User interface");
        resourceCodes.add("Manage screen");
        resourceCodes.add("Shopping screen");
        resourceCodes.add("Information screen");
    }

    private static List<Permission> provideListPermission() {
        List<Permission> permissions = new ArrayList<>();
        for (String resource : resourceCodes) {
            Permission permission = new Permission(resource, Scope.VIEW, resource, 1);
            permissions.add(permission);
        }
        return permissions;
    }

    private static List<RolePermission> provideListRolePermission() {
        List<RolePermission> rolePermissions = new ArrayList<>();
        for (String resource : resourceCodes) {
            RolePermission rolePermission = new RolePermission(roleId, resource, Scope.VIEW);
            rolePermissions.add(rolePermission);
        }
        return rolePermissions;
    }

    private static List<RolePermissionCreateCmd> provideListRolePermissionCreateCmd(List<String> resourceCodes) {
        List<Scope> scopes = new ArrayList<>();
        scopes.add(Scope.VIEW);
        scopes.add(Scope.CREATE);
        scopes.add(Scope.UPDATE);
        scopes.add(Scope.DELETE);
        List<RolePermissionCreateCmd> cmds = new ArrayList<>();
        for (String resource : resourceCodes) {
            cmds.add(new RolePermissionCreateCmd(resource, scopes));
        }
        return cmds;
    }

    static RoleCreateOrUpdateCmd provideRoleCreateOrUpdateCmd(Boolean root, boolean containRolePermission) {
        RoleCreateOrUpdateCmd cmd = RoleCreateOrUpdateCmd.builder().code(code).name(name).description(description)
                .isRoot(root).permissions(containRolePermission ? provideListRolePermissionCreateCmd(resourceCodes) : null).build();
        return cmd;
    }

    @Test
    @DisplayName("Test init Role by provideRoleCreateOrUpdateCmd, should pass if cmd obj fields equal with created Role fields")
    void testCreateRoleWithCmd() {
        RoleCreateOrUpdateCmd cmd = provideRoleCreateOrUpdateCmd(false, true);
        Role role = new Role(cmd, provideListPermission());

        List<RolePermissionCreateCmd> cmds = cmd.getPermissions();
        boolean isPermissionRolesMatchInputCmd = role.getPermissions().stream().anyMatch(rp -> cmds.stream().anyMatch(
                rpCmd -> rp.getResourceCode().equals(rpCmd.getResourceCode()) && rpCmd.getScopes().contains(rp.getScope())));

        List<Permission> provided = provideListPermission();
        boolean isPermissionRolesIncludedInExistedPermission = role.getPermissions().stream().allMatch(rp -> provided.stream().anyMatch(
                rpProvided -> rp.getResourceCode().equals(rpProvided.getResourceCode()) && rp.getScope().equals(rpProvided.getScope())));

        boolean checkPermissionList = isPermissionRolesIncludedInExistedPermission && isPermissionRolesMatchInputCmd;

        assertAll(() -> assertEquals(cmd.getCode(), role.getCode()), () -> assertEquals(cmd.getName(), role.getName()),
                () -> assertEquals(cmd.getDescription(), role.getDescription()), () -> assertEquals(RoleStatus.ACTIVE, role.getStatus()),
                () -> assertEquals(cmd.getIsRoot(), role.getIsRoot()), () -> assertEquals(false, role.getDeleted()),
                () -> assertNotNull(role.getPermissions()), () -> assertTrue(role.getPermissions().size() > 0),
                () -> assertTrue(checkPermissionList));
    }

    @Test
    @DisplayName("Test update Role by provideRoleCreateOrUpdateCmd, should pass if cmd obj fields equal with created Role fields and delete all " + "RolePermission list")
    void testUpdateRoleWithCmd() {
        RoleCreateOrUpdateCmd cmd = provideRoleCreateOrUpdateCmd(false, true);
        Role role = new Role(cmd, provideListPermission());

        List<String> resouces = new ArrayList<>();
        resouces.add("Product screen");
        resouces.add("User interface");

        RoleCreateOrUpdateCmd cmd2 = provideRoleCreateOrUpdateCmd(false, false);
        cmd2.setPermissions(provideListRolePermissionCreateCmd(resouces));

        role.update(cmd2, provideListPermission());

        List<RolePermissionCreateCmd> cmds = cmd2.getPermissions();
        boolean isPermissionRolesMatchInputCmd = role.getPermissions().stream().filter(rp -> !rp.getDeleted()).anyMatch(
                rp -> cmds.stream().anyMatch(
                        rpCmd -> rp.getResourceCode().equals(rpCmd.getResourceCode()) && rpCmd.getScopes().contains(rp.getScope())));

        List<Permission> provided = provideListPermission();
        boolean isPermissionRolesIncludedInExistedPermission = role.getPermissions().stream().allMatch(rp -> provided.stream().anyMatch(
                rpProvided -> rp.getResourceCode().equals(rpProvided.getResourceCode()) && rp.getScope().equals(rpProvided.getScope())));

        boolean checkPermissionList = isPermissionRolesIncludedInExistedPermission && isPermissionRolesMatchInputCmd;

        assertAll(() -> assertEquals(cmd.getCode(), role.getCode()), () -> assertEquals(cmd.getName(), role.getName()),
                () -> assertEquals(cmd.getDescription(), role.getDescription()), () -> assertEquals(RoleStatus.ACTIVE, role.getStatus()),
                () -> assertEquals(cmd.getIsRoot(), role.getIsRoot()), () -> assertEquals(false, role.getDeleted()),
                () -> assertNotNull(role.getPermissions()), () -> assertTrue(checkPermissionList),
                () -> assertEquals(3, role.getPermissions().stream().filter(rp -> rp.getDeleted()).count()));
    }

    @Test
    @DisplayName("Test delete Role, should pass if field deleted equal true and all RolePermission related get deleted")
    void testDeleteRole() {
        RoleCreateOrUpdateCmd cmd = provideRoleCreateOrUpdateCmd(true, true);
        Role role = new Role(cmd, provideListPermission());
        role.deleted();
        assertAll(() -> assertEquals(true, role.getDeleted()), () -> assertTrue(role.getPermissions().isEmpty()));
    }

    @Test
    @DisplayName("Test enrichPermissions, should pass if list RolePermission equal with input list")
    void testEnrichPermissions() {
        RoleCreateOrUpdateCmd cmd = provideRoleCreateOrUpdateCmd(true, false);
        Role role = new Role(cmd, provideListPermission());
        List<RolePermission> ls = provideListRolePermission();
        role.enrichPermissions(ls);
        assertEquals(ls, role.getPermissions());
    }

    @Test
    @DisplayName("Test method active, should pass if field status equal ACTIVE")
    void testActive() {
        RoleCreateOrUpdateCmd cmd = provideRoleCreateOrUpdateCmd(true, false);
        Role role = new Role(cmd, provideListPermission());
        role.active();
        assertEquals(RoleStatus.ACTIVE, role.getStatus());
    }

    @Test
    @DisplayName("Test method inactive, should pass if field status equal INACTIVE")
    void testInActive() {
        RoleCreateOrUpdateCmd cmd = provideRoleCreateOrUpdateCmd(true, false);
        Role role = new Role(cmd, provideListPermission());
        role.inactive();
        assertEquals(RoleStatus.INACTIVE, role.getStatus());
    }


}