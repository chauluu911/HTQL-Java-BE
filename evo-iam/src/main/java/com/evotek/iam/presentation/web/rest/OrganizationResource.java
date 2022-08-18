package com.evotek.iam.presentation.web.rest;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.iam.application.dto.request.*;
import com.evotek.iam.domain.Organization;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

@Api(tags = "Organization Resource")
@RequestMapping("/api")
@Validated
public interface OrganizationResource {

    @ApiOperation(value = "Create Organization")
    @PostMapping("/organizations")
    @PreAuthorize("hasPermission(null, 'organization:create')")
    Response<Organization> create(@RequestBody @Valid OrganizationCreateRequest request);

    @ApiOperation(value = "Update Organization")
    @PostMapping("/organizations/{id}/update")
    @PreAuthorize("hasPermission(null, 'organization:update')")
    Response<Organization> update(@PathVariable String id, @RequestBody @Valid OrganizationUpdateRequest request);

    @ApiOperation(value = "Delete Organization")
    @PostMapping("/organizations/{id}/delete")
    @PreAuthorize("hasPermission(null, 'organization:delete')")
    Response<Void> delete(@PathVariable String id);

    @ApiOperation(value = "Active Organization")
    @PostMapping("/organizations/{id}/active")
    @PreAuthorize("hasPermission(null, 'organization:update')")
    Response<Boolean> active(@PathVariable String id);

    @ApiOperation(value = "Search Organization")
    @GetMapping("/organizations/search")
    @PreAuthorize("hasPermission(null, 'organization:view')")
    PagingResponse<Organization> search(@ValidatePaging(allowedSorts = {"id", "createdAt", "name"}) OrganizationSearchRequest request);

    @ApiOperation(value = "Find Organization")
    @GetMapping("/organizations/{id}")
    @PreAuthorize("hasPermission(null, 'organization:view')")
    Response<Organization> getById(@PathVariable String id);

    @ApiOperation(value = "Find Organization by ids")
    @PostMapping("/organizations/find-by-ids")
    @PreAuthorize("hasPermission(null, 'organization:view')")
    Response<List<Organization>> findByIds(@RequestBody OrganizationGetByIdsRequest request);
    @ApiOperation(value = "Inactive Organization")
    @PostMapping("/organizations/{id}/inactive")
    @PreAuthorize("hasPermission(null, 'user:update')")
    Response<Boolean> inactive(@PathVariable String id);

    @ApiOperation(value = "Find organization by business code")
    @GetMapping("/organizations/find-by-business-code")
    @PreAuthorize("hasPermission(null, 'organization:view')")
    Response<Organization> findOrganizationByBusinessCode(@RequestBody String businessCode);
    @ApiOperation(value = "Find organization by email")
    @GetMapping("/organizations/find-by-email")
    @PreAuthorize("hasPermission(null, 'organization:view')")
    Response<Organization> findOrganizationEntityByEmail(@RequestBody String email);
}