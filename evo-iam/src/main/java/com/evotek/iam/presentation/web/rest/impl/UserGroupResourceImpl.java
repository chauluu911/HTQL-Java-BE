package com.evotek.iam.presentation.web.rest.impl;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.iam.application.dto.request.UserGroupCreateRequest;
import com.evotek.iam.application.dto.request.UserGroupMemberUpdateRequest;
import com.evotek.iam.application.dto.request.GroupSearchRequest;
import com.evotek.iam.application.dto.request.UserGroupUpdateRequest;
import com.evotek.iam.application.service.UserGroupService;
import com.evotek.iam.domain.UserGroup;
import com.evotek.iam.presentation.web.rest.UserGroupResource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@Slf4j
@RestController
public class UserGroupResourceImpl implements UserGroupResource {
    private final UserGroupService userGroupService;


    public UserGroupResourceImpl(UserGroupService userGroupService) {
        this.userGroupService = userGroupService;
    }

    @Override
    public Response<UserGroup> create(@Valid UserGroupCreateRequest request) {
        UserGroup userGroup = this.userGroupService.create(request);
        return Response.of(userGroup);
    }

    @Override
    public Response<UserGroup> update(String id, @Valid UserGroupUpdateRequest request) {
        UserGroup userGroup = this.userGroupService.update(id, request);
        return Response.of(userGroup);
    }

    @Override
    public Response<UserGroup> getUserGroupById(String id) {
        return Response.of(this.userGroupService.findById(id));
    }

    @Override
    public PagingResponse<UserGroup> search(GroupSearchRequest request) {
        return this.userGroupService.search(request);
    }

    @Override
    public Response<Void> addUsers(String id, UserGroupMemberUpdateRequest request) {
        this.userGroupService.addListUserToGroup(id, request);
        return Response.ok();
    }

    @Override
    public Response<Void> removeUsers(String id, UserGroupMemberUpdateRequest request) {
        this.userGroupService.removeListUserToGroup(id, request);
        return Response.ok();
    }

    @Override
    public Response<Void> delete(String id) {
        this.userGroupService.delete(id);
        return Response.ok();
    }

}
