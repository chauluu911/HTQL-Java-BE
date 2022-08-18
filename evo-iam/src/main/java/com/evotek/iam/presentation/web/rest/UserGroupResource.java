package com.evotek.iam.presentation.web.rest;


import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.iam.application.dto.request.UserGroupCreateRequest;
import com.evotek.iam.application.dto.request.UserGroupMemberUpdateRequest;
import com.evotek.iam.application.dto.request.GroupSearchRequest;
import com.evotek.iam.application.dto.request.UserGroupUpdateRequest;
import com.evotek.iam.domain.UserGroup;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@Api(tags = "Group User Resource")
@RequestMapping("/api")
@Validated
public interface UserGroupResource {
    @ApiOperation(value = "Create group user")
    @PostMapping("/user-group")
    @PreAuthorize("hasPermission(null, 'user_group:create')")
    Response<UserGroup> create(@RequestBody @Valid UserGroupCreateRequest request);

    @ApiOperation(value = "Update group user")
    @PostMapping("/user-group/{id}/update")
    @PreAuthorize("hasPermission(null, 'user_group:update')")
    Response<UserGroup> update(@PathVariable String id, @RequestBody @Valid UserGroupUpdateRequest request);

    @ApiOperation(value = "Get group user by Id")
    @GetMapping("/user-group/{id}")
    @PreAuthorize("hasPermission(null, 'user_group:view')")
    Response<UserGroup> getUserGroupById(@PathVariable String id);

    @ApiOperation(value = "Search group user")
    @GetMapping("/user-group/search")
    @PreAuthorize("hasPermission(null, 'user_group:view')")
    PagingResponse<UserGroup> search(@ValidatePaging(allowedSorts = {"lastModifiedAt", "createdAt", "name", "description"}) GroupSearchRequest request);

    @ApiOperation(value = "Add user to group ")
    @PostMapping("/user-group/{id}/add-users")
    @PreAuthorize("hasPermission(null, 'user_group:update')")
    Response<Void> addUsers(@PathVariable String id, @RequestBody @Valid UserGroupMemberUpdateRequest request);

    @ApiOperation(value = "Remove user to group ")
    @PostMapping("/user-group/{id}/remove-users")
    @PreAuthorize("hasPermission(null, 'user_group:delete')")
    Response<Void> removeUsers(@PathVariable String id, @RequestBody @Valid UserGroupMemberUpdateRequest request);

    @ApiOperation(value = "Remove Group User")
    @PostMapping("/user-group/{id}/delete")
    @PreAuthorize("hasPermission(null, 'user_group:delete')")
    Response<Void> delete(@PathVariable String id);

}
