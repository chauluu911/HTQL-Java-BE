package com.evotek.common.ldapcore.service;

import com.evotek.common.ldapcore.dto.request.UserLdapChangePasswordRequest;
import com.evotek.common.ldapcore.dto.request.UserLdapCreateOrUpdateRequest;
import com.evotek.common.ldapcore.dto.response.UserLdapResponse;

import java.util.List;

public interface LDAPService {

    String MEMBER_OF_ATTRIBUTE = "memberOf";
    String COMMON_NAME = "CN";
    String SAM_ACCOUNT_NAME = "sAMAccountName";
    String USER_NAME = "userName";

    Boolean authenticateUserLDAP(String username, String password);

    Boolean authenticateUserLDAPInGroup(String username, String password);

    List<UserLdapResponse> getAllUserInLdap();

    void createUserLdap(UserLdapCreateOrUpdateRequest request);

    List<UserLdapResponse> getAllUserInGroupLdap();

    boolean resetPassUserLdap(String cn, UserLdapChangePasswordRequest request);
}
