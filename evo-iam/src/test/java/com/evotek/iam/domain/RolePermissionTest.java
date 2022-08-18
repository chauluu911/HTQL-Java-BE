package com.evotek.iam.domain;

import com.evotek.common.enums.Scope;
import com.evotek.common.util.IdUtils;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

class RolePermissionTest {

    private static final String roleId = IdUtils.nextId();
    private static final String resourceCode = "testResourceCode";

    @Test
    @DisplayName("Test create Permission with constructor, should pass if Permission created fields equal with input fields")
    void testCreateRolePermissionWithConstructor() {
        RolePermission rolePermission = new RolePermission(roleId, resourceCode, Scope.VIEW);
        assertAll(() -> assertEquals(resourceCode, rolePermission.getResourceCode()), () -> assertEquals(roleId, rolePermission.getRoleId()),
                () -> assertEquals(false, rolePermission.getDeleted()));
    }

    @Test
    @DisplayName("Test delete Permission, should pass if field deleted equals true")
    void testDeletePermission() {
        RolePermission rolePermission = new RolePermission(roleId, resourceCode, Scope.VIEW);
        rolePermission.deleted();
        assertEquals(true, rolePermission.getDeleted());
    }

    @Test
    @DisplayName("Test undelete Permission, should pass if field deleted equals false")
    void testUnDeletePermission() {
        RolePermission rolePermission = new RolePermission(roleId, resourceCode, Scope.VIEW);
        rolePermission.unDelete();
        assertEquals(false, rolePermission.getDeleted());
    }


}