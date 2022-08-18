package com.evotek.iam.infrastructure.support.exception;

import com.evotek.common.error.ResponseError;
import lombok.Getter;

@Getter
public enum BadRequestError implements ResponseError {
    USER_EMAIL_EXITED(40001001, "Email exited"),
    USER_USERNAME_EXITED(40001002, "Username exited"),
    WRONG_PASSWORD(40001004, "Wrong password"),
    USER_INVALID(40001005, "User invalid: {0}"),
    STATUS_INVALID(40001006, "Status invalid: {0}"),
    SYNC_VIDEO_IN_CHANNEL_ERROR(40001007, "Sync video in channel error"),
    DEPARTMENT_CODE_EXISTED(40001008, "DepartmentCode exited"),
    DEPARTMENT_NOT_EXITED(40001009, "Department not exited"),
    UPDATE_DEPARTMENT_NOT_SUPPORTED(40001050, "Update department not supported"),
    ORG_UNIT_NOT_EXITED(40001010, "OrgUnit not exited"),
    ORG_UNIT_CODE_EXITED(40001011, "OrgUnit Code exited"),
    ORG_UNIT_CODE_NOT_EXITED(40001012, "OrgUnit Code not exited"),
    IDS_IS_REQUIRED(40001001, "Ids is required"),
    LOGIN_FAIL_BLOCK_ACCOUNT(40001013, "Login fail due account was block!"),
    LOGIN_FAIL_WARNING_BEFORE_BLOCK(40001014, "Warning account will be block!"),
    PASSWORD_REQUIRED(40001015, "Password is required"),
    CHANGE_PASSWORD_NOT_SUPPORTED(40001016, "Change password is supported"),
    USER_LDAP_NOT_EXISTED(40001017, "User LDAP not existed"),
    CLIENT_NOT_EXISTED(40001018, "Client not exited"),
    ORGANIZATION_CODE_EXITED(40001019, "Organization code exited"),
    PASSWORD_NOT_STRONG(40001020, "Password required strong"),
    ORGANIZATION_LEASING_UNIT_NOT_EXITED(40001021, "Organization not leasing this unit"),
    UNIT_IN_USE(40001024, "Unit is in use"),
    CONTACT_ORGANIZATION_NOT_EXISTED(40001025, "Contact Organization not existed"),
    USER_PHONE_NUMBER_EXITED(40001028, "Phone number exited"),
    USER_EMPLOYEE_CODE_EXITED(40001029, "Employee code exited"),
    CLIENT_NAME_EXITED(40001030, "Client name exited"),
    CANNOT_DELETE_ACTIVE_USER(40001031, "Can not delete because user is active"),
    CANNOT_DELETE_DELETED_USER(40001032, "User has been deleted"),
    EMAIL_NOT_EXISTED_IN_SYSTEM(40001033, "Email not existed in system"),
    REPEAT_PASSWORD_DOES_NOT_MATCH(40001034, "Repeat password does not match"),
    ROLE_INVALID(40001035, "Role is invalid"),
    FLOOR_NOT_FOUND(40001036, "Floor not found"),
    IDS_INVALID(40001037, "Ids invalid"),
    ADMIN_MUST_CENTER_LEVEL(40001040, "Role admin must be center level"),
    BUILDING_ID_REQUIRED(40001041, "Building id required"),
    ACCOUNT_NOT_PERMISSION_ON_WEB(40001042, "Account not permission on web"),
    ORGANIZATION_CAN_NOT_INACTIVE_DUE_HAVE_GROUND_USING(40001043, "Organization can not inactive due have ground using"),
    BUSINESS_CODE_EXISTED(40001046, "Business code exited"),
    FILE_NOT_EXISTED(40001047, "Avatar file not existed"),
    ACCOUNT_EMPLOYEE_CAN_NOT_CHANGE_PASSWORD(40001048, "Account employee can not execute the function changing password"),
    ORG_NAME_EXISTED(40001049, "Org name existed"),
    ROLE_CODE_EXISTED(40001050, "Role code Existed"),
    ADMIN_ROLE_ALREADY_EXISTED(40001051, "An administrator role already exists"),
    EMPLOYEE_NOT_FOUND(40001052, "Employee not found"),
    DEPARTMENT_IDS_REQUIRED(40001053, "Department ids required"),
    CONNECT_LDAP_FAIL(40001054, "CONNECT LDAP FAIL"),
    CREATE_USER_LDAP_FAIL(40001055, "Create user LDAP fail"),
    CLIENT_CANT_CHANGE_STATUS(40001056, "Client can't inactivate due have user using"),
    JOB_TITLE_CODE_EXISTED(40001057, "JobTitleCode exited"),
    JOB_TITLE_DOES_NOT_EXISTED(40001058, "Job Title does not exited"),
    JOB_TITLE_ID_REQUIRED(40001059, "Job Title id required"),
    JOB_TITLE_ALREADY_IN_USED(40001060, "Job Title already in used"),
    ACCOUNT_LDAP_CAN_NOT_CHANGE_PASSWORD(40001057, "Account LDAP can not execute the function changing password"),
    GROUP_LDAP_CAN_NOT_SYNC(40001058, "Group LDAP can not sync"),
    ADD_USER_TO_GROUP_NOT_SUPPORTED(40001059, "Add user to group not supported"),
    USER_IS_ALREADY_IN_GROUP(40001060, "User is already in group"),
    USER_MEMBER_INVALID(40001061, "User member invalid"),
    USER_IS_NOT_IN_GROUP(40001060, "User is not already in group"),
    CREATE_GROUP_LDAP_NOT_SUPPORT(40001061, "Create Group LDAP not support"),
    REMOVE_USER_FROM_GROUP_NOT_SUPPORTED(40001059, "Remove user from group not supported"),
    CANNOT_DELETE_DELETED_ORGANIZATION(40001057, "Organization has been deleted"),
    ORGANIZATION_EMAIL_EXITED(40001058, "Email exited"),
    ORGANIZATION_NOT_FOUND(40001059, "Organization not found")
    ;
    private final Integer code;
    private final String message;

    BadRequestError(Integer code, String message) {
        this.code = code;
        this.message = message;
    }

    @Override
    public String getName() {
        return name();
    }

    @Override
    public String getMessage() {
        return message;
    }

    @Override
    public int getStatus() {
        return 400;
    }

    @Override
    public Integer getCode() {
        return code;
    }
}
