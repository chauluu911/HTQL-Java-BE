package com.evotek.iam.domain;

import com.evotek.common.enums.Scope;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

class PermissionTest {

    private static final String resourceCode = "testResouceCode";
    private static final String name = "testName";
    private static final String resourceName = "testResourceName";

    @Test
    @DisplayName("Test create Permission with constructor, should pass if Permission created fields equal with input fields")
    void testCreatePermissionWithConstructor() {
        Permission permission = new Permission(resourceCode, Scope.VIEW, name, 1);
        assertAll(() -> assertEquals(resourceCode, permission.getResourceCode()), () -> assertEquals(name, permission.getName()),
                () -> assertEquals(Scope.VIEW, permission.getScope()), () -> assertEquals(false, permission.getDeleted()));
    }

    @Test
    @DisplayName("Test delete Permission, should pass if field deleted equals true")
    void testDeletePermission() {
        Permission permission = new Permission(resourceCode, Scope.VIEW, name, 1);
        permission.deleted();
        assertEquals(true, permission.getDeleted());
    }

    @Test
    @DisplayName("Test enrichResourceName, should pass if field resourceName equals with input")
    void testEnrichResourceName() {
        Permission permission = new Permission(resourceCode, Scope.VIEW, name, 1);
        permission.enrichResourceName(resourceName);
        assertEquals(resourceName, permission.getResourceName());
    }


}