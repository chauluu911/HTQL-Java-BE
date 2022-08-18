package com.evotek.asset.presentation.web.rest;

import com.evotek.asset.application.dto.request.AssetTypeCreateRequest;
import com.evotek.asset.application.dto.request.AssetTypeSearchRequest;
import com.evotek.asset.application.dto.request.AssetTypeUpdateRequest;
import com.evotek.asset.domain.AssetType;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@Api(tags = "Asset type Resource")
@RequestMapping("/api/asset-types")
@Validated
public interface AssetTypeResource {
    @ApiOperation(value = "Create Asset type")
    @PostMapping("")
    @PreAuthorize("hasPermission(null, 'asset:create')")
    public Response<AssetType> create(@RequestBody @Valid AssetTypeCreateRequest request);

    @ApiOperation(value = "Update asset type")
    @PostMapping("/{id}/update")
    @PreAuthorize("hasPermission(null, 'asset:update')")
    public Response<AssetType> update(@PathVariable String id, @Valid @RequestBody AssetTypeUpdateRequest request);

    @ApiOperation(value = "Get asset type by id")
    @GetMapping("/{id}")
    @PreAuthorize("hasPermission(null,'asset:update')")
    public Response<AssetType> getAssetTypeById(@PathVariable String id);

    @ApiOperation(value = "Active asset type")
    @PostMapping("/{id}/active")
    @PreAuthorize("hasPermission(null, 'asset:update')")
    Response<Boolean> active(@PathVariable String id);

    @ApiOperation(value = "Inactive asset type")
    @PostMapping("{id}/inactive")
    @PreAuthorize("hasPermission(null, 'asset:update')")
    Response<Boolean> inactive(@PathVariable String id);

    @ApiOperation(value = "Delete asset type")
    @PostMapping("{id}/delete")
    @PreAuthorize("hasPermission(null, 'asset:delete')")
    Response<Boolean> delete(@PathVariable String id);

    @ApiOperation(value = "search asset type")
    @GetMapping("/search")
    @PreAuthorize("hasPermission(null, 'asset:view')")
    PagingResponse<AssetType> search(@ValidatePaging(allowedSorts = {"code, name, lastModifiedAt, createdAt"}) AssetTypeSearchRequest request);

    @ApiOperation(value = "auto complete asset type")
    @GetMapping("/auto-complete")
    @PreAuthorize("hasPermission(null, 'asset:view')")
    PagingResponse<AssetType> autoComplete(@ValidatePaging(allowedSorts = {"code, name, lastModifiedAt, createdAt"}) AssetTypeSearchRequest request);
}
