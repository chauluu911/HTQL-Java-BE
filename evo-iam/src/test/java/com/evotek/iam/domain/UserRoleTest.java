package com.evotek.iam.domain;

import com.evotek.common.util.IdUtils;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class UserRoleTest {

    static String userId;
    static String roleId;

    @BeforeAll
    private static void init() {
        userId = IdUtils.nextId();
        roleId = IdUtils.nextId();
    }

    @Test
    @DisplayName("Test create UserRole by userId and roleId, should pass if created UserRole fields equal with two fields")
    void constructorUserRole() {
        UserRole userRole = new UserRole(userId, roleId);
        assertAll(() -> assertEquals(userRole.getRoleId(), roleId),
                () -> assertEquals(userRole.getUserId(), userId),
                () -> assertFalse(userRole.getDeleted()));
    }

    @Test
    @DisplayName("Test delete UserRole, should pass field deleted is true")
    void unDeleteUserRole() {
        UserRole userRole = new UserRole(userId, roleId);
        userRole.deleted();
        assertTrue(userRole.getDeleted());
    }

    @Test
    @DisplayName("Test UnDelete UserRole, should pass field deleted is false")
    void deleteUserRole() {
        UserRole userRole = new UserRole(userId, roleId);
        userRole.unDelete();
        assertFalse(userRole.getDeleted());
    }


}
