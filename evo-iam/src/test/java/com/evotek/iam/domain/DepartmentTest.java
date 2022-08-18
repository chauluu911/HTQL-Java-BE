package com.evotek.iam.domain;

import com.evotek.iam.domain.command.DepartmentCreateCmd;
import com.evotek.iam.domain.command.DepartmentUpdateCmd;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class DepartmentTest {

    @ParameterizedTest
    @MethodSource("provideCreateDepartmentCmd")
    @DisplayName("Test create department by DepartmentCreateCmd obj, should pass if created department fields equal with cmd fields")
    void createDepartmentByCmd(DepartmentCreateCmd cmd) {
        Department department = new Department(cmd, null);
        assertAll(() -> assertEquals(department.getName(), cmd.getName()), () -> assertEquals(department.getCode(), cmd.getCode()),
                () -> assertEquals(department.getParentId(), cmd.getParentId()), () -> assertEquals(department.getDescription(),
                        cmd.getDescription()), () -> assertEquals(false, department.getDeleted()));
    }

    @ParameterizedTest
    @MethodSource("provideCreateDepartmentCmd")
    @DisplayName("Test update department by UpdateDepartmentCmd, should pass if created department fields equal with update cmd fields")
    void updateDepartmentByCmd(DepartmentCreateCmd cmd) {
        Department department = new Department(cmd, null);
        DepartmentUpdateCmd updateCmd = new DepartmentUpdateCmd("testName", "testParentId", "testDescription");
        department.update(updateCmd, null);
        assertAll(() -> assertEquals(department.getName(), cmd.getName()), () -> assertEquals(department.getDescription(), cmd.getDescription()));
    }

    @ParameterizedTest
    @MethodSource("provideCreateDepartmentCmd")
    @DisplayName("Test delete department,should return true if field deleted of department is true ")
    void deleteDepartment(DepartmentCreateCmd cmd) {
        Department department = new Department(cmd, null);
        department.delete();
        assertTrue(department.getDeleted());
    }

    @ParameterizedTest
    @MethodSource("provideCreateDepartmentCmd")
    @DisplayName("Test update parentPath, should pass if new parentPath equals with given parentPath string ")
    void updateParentPath(DepartmentCreateCmd cmd) {
        Department department = new Department(cmd, null);
        String newParentPath = "testNewParentPath";
        department.updateParentPath(newParentPath);
        assertEquals(department.getParentPath(), newParentPath);
    }

    private static Stream<Object> provideCreateDepartmentCmd() {
        DepartmentCreateCmd cmd = new DepartmentCreateCmd("testName", "testCode", "testParentId", "testDescription");
        return Stream.of(cmd);
    }

}
