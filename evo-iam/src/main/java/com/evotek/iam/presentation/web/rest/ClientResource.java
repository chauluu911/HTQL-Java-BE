package com.evotek.iam.presentation.web.rest;

import com.evotek.common.UserAuthority;
import com.evotek.common.dto.request.iam.ClientLoginRequest;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.iam.ClientToken;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.iam.application.dto.request.ClientCreateRequest;
import com.evotek.iam.application.dto.request.ClientSearchRequest;
import com.evotek.iam.application.dto.request.ClientUpdateRequest;
import com.evotek.iam.application.dto.response.ClientResponse;
import com.evotek.iam.domain.Client;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@Api(tags = "Client Resource")
@RequestMapping("/api")
@Validated
public interface ClientResource {

    @ApiOperation(value = "Client authentication")
    @PostMapping("/client/authenticate")
    Response<ClientToken> clientAuthentication(@RequestBody @Valid ClientLoginRequest request);

    @ApiOperation(value = "Get my authorities")
    @GetMapping("/clients/me/authorities")
    Response<UserAuthority> getAuthoritiesByClientId();

    @ApiOperation(value = "Create Client")
    @PostMapping("/clients")
    @PreAuthorize("hasPermission(null, 'client:create')")
    Response<ClientResponse> createClient(@RequestBody @Valid ClientCreateRequest request);

    @ApiOperation(value = "Update Client")
    @PostMapping("/clients/{id}/update")
    @PreAuthorize("hasPermission(null, 'client:update')")
    Response<Client> updateClient(@PathVariable String id, @RequestBody @Valid ClientUpdateRequest request);

    @ApiOperation(value = "Search client")
    @GetMapping("/clients")
    @PreAuthorize("hasPermission(null, 'client:view')")
    PagingResponse<Client> search(@ValidatePaging(allowedSorts = {"name"}) ClientSearchRequest request);

    @ApiOperation(value = "get by id client")
    @GetMapping("/clients/{id}")
    @PreAuthorize("hasPermission(null, 'client:view')")
    Response<Client> getById(@PathVariable String id);

    @ApiOperation(value = "Active client")
    @PostMapping("/clients/{id}/active")
    @PreAuthorize("hasPermission(null, 'client:update')")
    Response<Client> active(@PathVariable String id);


    @ApiOperation(value = "Inactive client by id")
    @PostMapping("/clients/{id}/inactive")
    @PreAuthorize("hasPermission(null, 'client:update')")
    Response<Client> inactive(@PathVariable String id);
}
