package com.evotek.iam.presentation.web.rest.impl;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.iam.application.dto.request.RoleAutocompleteRequest;
import com.evotek.iam.application.dto.request.RoleCreateRequest;
import com.evotek.iam.application.dto.request.RoleSearchRequest;
import com.evotek.iam.application.dto.request.RoleUpdateRequest;
import com.evotek.iam.application.service.RoleService;
import com.evotek.iam.domain.Role;
import com.evotek.iam.presentation.web.rest.RoleResource;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
public class RoleResourceImpl implements RoleResource {

    private final RoleService roleService;

    public RoleResourceImpl(RoleService roleService) {
        this.roleService = roleService;
    }

    @Override
    public PagingResponse<Role> search(RoleSearchRequest request) {
        return PagingResponse.of(roleService.searchByRoleRequest(request));
    }

    @Override
    public PagingResponse<Role> autocomplete(RoleAutocompleteRequest request) {
        return PagingResponse.of(roleService.autocomplete(request));
    }

    @Override
    public Response<Role> create(@Valid RoleCreateRequest request) {
        Role role = this.roleService.create(request);
        return Response.of(role);
    }

    @Override
    public Response<Role> update(String id, @Valid RoleUpdateRequest request) {
        Role role = this.roleService.update(id, request);
        return Response.of(role);
    }

    @Override
    public Response<Role> getById(String id) {
        Role role = this.roleService.getById(id);
        return Response.of(role);
    }

    @Override
    public Response<Role> delete(String id) {
        this.roleService.delete(id);
        return Response.ok();
    }

    @Override
    public Response<Boolean> active(String id) {
        this.roleService.active(id);
        return Response.ok();
    }

    @Override
    public Response<Boolean> inactive(String id) {
        this.roleService.inactive(id);
        return Response.ok();
    }
}
