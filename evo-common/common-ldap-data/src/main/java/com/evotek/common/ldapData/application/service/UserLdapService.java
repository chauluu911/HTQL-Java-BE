package com.evotek.common.ldapData.application.service;

import com.evotek.common.ldapData.application.dto.request.UserLdapCreateOrUpdateRequest;
import com.evotek.common.ldapData.domain.UserLdap;

import java.util.List;

public interface UserLdapService {
    List<UserLdap> getAllUserInLdap();

    UserLdap findUserLdapById(String id);


    void resetPassword(String username, String password);

    UserLdap create(UserLdapCreateOrUpdateRequest request);

    UserLdap update(String userId, UserLdapCreateOrUpdateRequest request);

    Boolean authenticate(String username, String password);


}
