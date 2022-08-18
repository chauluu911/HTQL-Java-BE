package com.evotek.iam.domain;

import com.evotek.common.enums.Gender;
import com.evotek.common.enums.Scope;
import com.evotek.common.enums.UserLevel;
import com.evotek.common.util.IdUtils;
import com.evotek.iam.domain.command.UserInternalCreateCmd;
import com.evotek.iam.infrastructure.support.enums.AuthenticationType;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

class UserTest {
    private static User user;

    private static String roleId1;

    private static final List<Role> roles = new ArrayList<>();

    private static final String RESOURCE_CODE = "iam";

    private static final String ROLE_NAME = "Quản lý tòa nhà";
    private static final String ROLE_CODE = "ROLE_MN";

    private static final String USERNAME = "admin";
    private static final String PASSWORD = "123456";
    private static final String FULL_NAME = "Admin";
    private static final String EMAIL = "admin@gmail.com";
    private static final String PHONE_NUMBER = "0888676867";
    private static final AuthenticationType AUTHENTICATION_TYPE = AuthenticationType.LDAP;
    private static final String EMPLOYEE_CODE = "MB001";
    private static final String DEPARTMENT_NAME = "Phong Nhan su";
    private static final Gender GENDER = Gender.OTHER;

    @BeforeAll
    private static void init() {

        // init user
        user = User.builder()
                .username(USERNAME)
                .email(EMAIL)
                .gender(GENDER)
                .fullName(FULL_NAME)
                .password(PASSWORD)
                .authenticationType(AUTHENTICATION_TYPE)
                .employeeCode(EMPLOYEE_CODE)
                .build();

        // init role
        roleId1 = IdUtils.nextId();
        String roleId2 = IdUtils.nextId();
        RolePermission rolePermission1 = RolePermission.builder()
                .roleId(roleId1)
                .resourceCode(RESOURCE_CODE)
                .scope(Scope.CREATE)
                .build();

        RolePermission rolePermission2 = RolePermission.builder()
                .roleId(roleId2)
                .resourceCode(RESOURCE_CODE)
                .scope(Scope.CREATE)
                .build();

        Role role1 = Role.builder()
                .id(roleId1)
                .name(ROLE_NAME)
                .code(ROLE_CODE)
                .permissions(Collections.singletonList(rolePermission1))
                .isRoot(Boolean.FALSE)
                .build();

        Role role2 = Role.builder()
                .id(roleId2)
                .name(ROLE_NAME)
                .code(ROLE_CODE)
                .permissions(Collections.singletonList(rolePermission2))
                .isRoot(Boolean.FALSE)
                .build();

        roles.add(role1);
        roles.add(role2);
    }

    @Test
    @DisplayName("Test create User Internal, should pass if created User fields equal some fields")
    void testCreateUserInternal() {
        UserInternalCreateCmd cmd = UserInternalCreateCmd.builder()
                .phoneNumber(PHONE_NUMBER)
                .email(EMAIL)
                .username(USERNAME)
                .password(PASSWORD)
                .roleIds(Collections.singletonList(roleId1))
                .authenticationType(AuthenticationType.INTERNAL)
                .fullName(FULL_NAME)
                .gender(GENDER)
                .employeeCode(EMPLOYEE_CODE)
                .build();
        User user = new User(cmd, roles);
        assertAll(() -> assertTrue(roles.stream().map(Role::getId)
                        .collect(Collectors.toList())
                        .contains(user.getUserRoles().get(0).getRoleId())),
                () -> assertEquals(AuthenticationType.INTERNAL, user.getAuthenticationType()),
                () -> assertEquals(GENDER, user.getGender())
                );
    }


    @Test
    @DisplayName("Test create User Customer, should pass if created User fields equal some fields")
    void testCreateUserCustomer() {
        UserInternalCreateCmd cmd = UserInternalCreateCmd.builder()
                .phoneNumber(PHONE_NUMBER)
                .email(EMAIL)
                .username(USERNAME)
                .password(PASSWORD)
                .roleIds(Collections.singletonList(roleId1))
                .authenticationType(AuthenticationType.INTERNAL)
                .fullName(FULL_NAME)
                .gender(GENDER)
                .employeeCode(EMPLOYEE_CODE)
                .build();
        User user = new User(cmd, roles);
        assertAll(() -> assertTrue(roles.stream().map(Role::getId)
                        .collect(Collectors.toList())
                        .contains(user.getUserRoles().get(0).getRoleId())),
                () -> assertEquals(AuthenticationType.INTERNAL, user.getAuthenticationType()),
                () -> assertEquals(GENDER, user.getGender())
        );
    }

    // user contract, user is the same

    @Test
    @DisplayName("Test assign enrich role, should pass if created User fields equal some fields")
    void testEnrichRoles() {
        Role role = Role.builder()
                .id(IdUtils.nextId())
                .isRoot(Boolean.TRUE)
                .name("CENTER")
                .code("CENTER")
                .build();
        user.enrichRoles(Collections.singletonList(role));
        assertTrue(user.getRoles().stream().map(Role::getId).collect(Collectors.toList()).contains(role.getId()));
    }

}
