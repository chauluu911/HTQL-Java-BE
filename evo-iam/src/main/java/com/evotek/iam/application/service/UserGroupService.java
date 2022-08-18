package com.evotek.iam.application.service;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.iam.application.dto.request.UserGroupMemberUpdateRequest;
import com.evotek.iam.application.dto.request.GroupSearchRequest;
import com.evotek.iam.application.dto.request.UserGroupCreateRequest;
import com.evotek.iam.application.dto.request.UserGroupUpdateRequest;
import com.evotek.iam.domain.UserGroup;

public interface UserGroupService {
    PagingResponse<UserGroup> search(GroupSearchRequest request);

    UserGroup create(UserGroupCreateRequest request);

    UserGroup update(String groupId, UserGroupUpdateRequest request);

    void delete(String groupId);

    UserGroup findById(String groupId);

    void addListUserToGroup(String groupId, UserGroupMemberUpdateRequest request);

    void removeListUserToGroup(String groupId, UserGroupMemberUpdateRequest request);
}
