package com.evotek.iam.presentation.web.rest;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.iam.application.dto.request.RoleAutocompleteRequest;
import com.evotek.iam.application.dto.request.RoleCreateRequest;
import com.evotek.iam.application.dto.request.RoleSearchRequest;
import com.evotek.iam.application.dto.request.RoleUpdateRequest;
import com.evotek.iam.domain.Role;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@Api(tags = "Role Resource")
@RequestMapping("/api")
@Validated
public interface RoleResource {

    @ApiOperation(value = "Search Role")
    @GetMapping("/roles")
    @PreAuthorize("hasPermission(null, 'role:view')")
    PagingResponse<Role> search(@ValidatePaging(allowedSorts = {"id", "createdAt", "code", "name", "createdBy", "lastModifiedAt"})
                                        RoleSearchRequest request);

    @ApiOperation(value = "Search role auto complete")
    @GetMapping("/roles/auto-complete")
    @PreAuthorize("hasPermission(null, 'role:view')")
    PagingResponse<Role> autocomplete(@ValidatePaging(allowedSorts = {"code", "name"})
                                              RoleAutocompleteRequest request);

    @ApiOperation(value = "Create role")
    @PostMapping("/roles")
    @PreAuthorize("hasPermission(null, 'role:create')")
    Response<Role> create(@RequestBody @Valid RoleCreateRequest request);

    @ApiOperation(value = "Update role")
    @PostMapping("/roles/{id}/update")
    @PreAuthorize("hasPermission(null, 'role:update')")
    Response<Role> update(@PathVariable String id, @RequestBody @Valid RoleUpdateRequest request);

    @ApiOperation(value = "Find role")
    @GetMapping("/roles/{id}")
    @PreAuthorize("hasPermission(null, 'role:view')")
    Response<Role> getById(@PathVariable String id);

    @ApiOperation(value = "Delete role")
    @PostMapping("/roles/{id}/delete")
    @PreAuthorize("hasPermission(null, 'role:delete')")
    Response<Role> delete(@PathVariable String id);

    @ApiOperation(value = "Active Role")
    @PostMapping("/roles/{id}/active")
    @PreAuthorize("hasPermission(null, 'role:update')")
    Response<Boolean> active(@PathVariable String id);

    @ApiOperation(value = "Inactive Role")
    @PostMapping("/roles/{id}/inactive")
    @PreAuthorize("hasPermission(null, 'role:update')")
    Response<Boolean> inactive(@PathVariable String id);
}
