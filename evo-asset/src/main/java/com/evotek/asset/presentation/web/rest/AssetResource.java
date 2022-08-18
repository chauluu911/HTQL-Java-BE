package com.evotek.asset.presentation.web.rest;

import com.evotek.asset.application.dto.request.AssetCreateRequest;
import com.evotek.asset.application.dto.request.AssetSearchRequest;
import com.evotek.asset.application.dto.request.AssetUpdateRequest;
import com.evotek.asset.domain.Asset;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@Api(tags = "Asset Resource")
@RequestMapping("/api/assets")
@Validated
public interface AssetResource {
    @ApiOperation(value = "Create Asset")
    @PostMapping("")
    @PreAuthorize("hasPermission(null, 'asset:create')")
    public Response<Asset> create(@RequestBody @Valid AssetCreateRequest request);

    @ApiOperation(value = "update asset")
    @PostMapping("/{id}/update")
    @PreAuthorize("hasPermission(null,'asset:update')")
    public Response<Asset> update(@PathVariable("id") String id, @RequestBody @Valid AssetUpdateRequest request);

    @ApiOperation(value = "get asset by ID")
    @GetMapping("/{id}")
    @PreAuthorize("hasPermission(null,'asset:view')")
    public Response<Asset> getAssetById(@PathVariable("id") String id);

    @ApiOperation(value = "search asset")
    @GetMapping("/search")
    @PreAuthorize("hasPermission(null,'asset:view')")
    public PagingResponse<Asset> search(@ValidatePaging(allowedSorts = {"code, name, lastModifiedAt, createdAt"}) AssetSearchRequest request);

    @ApiOperation(value = "auto complete asset")
    @GetMapping("/auto-complete")
    @PreAuthorize("hasPermission(null,'asset:view')")
    public PagingResponse<Asset> autoComplete(@ValidatePaging(allowedSorts = {"code, name, lastModifiedAt, createdAt"}) AssetSearchRequest request);
}
