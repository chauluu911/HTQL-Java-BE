package com.evotek.common.ldapData.application.service;

import com.evotek.common.ldapData.application.dto.request.GroupLdapCreateOrUpdateRequest;
import com.evotek.common.ldapData.domain.GroupLdap;

import java.util.List;

public interface GroupLdapService {
    List<GroupLdap> getAllGroupLdap();

    GroupLdap create(GroupLdapCreateOrUpdateRequest request);

    GroupLdap update(String groupId, GroupLdapCreateOrUpdateRequest request);

    void delete(String groupId);

    GroupLdap findGroupById(String groupId);

    void addUserToGroup(String userId, String groupId);

}
