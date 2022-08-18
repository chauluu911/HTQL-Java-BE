package com.evotek.iam.presentation.web.rest.impl;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.iam.application.dto.request.*;
import com.evotek.iam.application.service.OrganizationService;
import com.evotek.iam.domain.Organization;
import com.evotek.iam.presentation.web.rest.OrganizationResource;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import java.util.List;
import java.util.Optional;

@RestController
public class OrganizationResourceImpl implements OrganizationResource {

    private final OrganizationService organizationService;

    public OrganizationResourceImpl(OrganizationService organizationService) {
        this.organizationService = organizationService;
    }

    @Override
    public PagingResponse<Organization> search(OrganizationSearchRequest request) {
        return PagingResponse.of(organizationService.search(request));
    }

    @Override
    public Response<Organization> getById(String id) {
        Organization organization = organizationService.getById(id);
        return Response.of(organization);
    }

    @Override
    public Response<List<Organization>> findByIds(OrganizationGetByIdsRequest request) {
        List<Organization> organizations = organizationService.findByIds(request);
        return Response.of(organizations);
    }

    @Override
    public Response<Organization> create(@Valid OrganizationCreateRequest request) {
        Organization organization = organizationService.create(request);
        return Response.of(organization);
    }

    @Override
    public Response<Organization> update(String id, @Valid OrganizationUpdateRequest request) {
        Organization organization = organizationService.update(id, request);
        return Response.of(organization);
    }
    @Override
    public Response<Void> delete(String id) {
        this.organizationService.delete(id);
        return Response.ok();
    }

    @Override
    public Response<Boolean> active(String id) {
        organizationService.active(id);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public Response<Boolean> inactive(String id) {
        organizationService.inactive(id);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public Response<Organization> findOrganizationByBusinessCode(String businessCode) {
        Optional<Organization> optionalOrganization = organizationService.findOrganizationByBusinessCode(businessCode);
        if (optionalOrganization.isEmpty()) {
            Response<Organization> response = new Response<>();
            response.setSuccess(false);
            response.setCode(HttpStatus.NOT_FOUND.value());
            return response;
        } else {
            return Response.of(optionalOrganization.get());
        }
    }

    @Override
    public Response<Organization> findOrganizationEntityByEmail(String email) {
        Optional<Organization> optionalOrganization = organizationService.findOrganizationEntityByEmail(email);
        if (optionalOrganization.isEmpty()) {
            Response<Organization> response = new Response<>();
            response.setSuccess(false);
            response.setCode(HttpStatus.NOT_FOUND.value());
            return response;
        } else {
            return Response.of(optionalOrganization.get());
        }
    }
}

