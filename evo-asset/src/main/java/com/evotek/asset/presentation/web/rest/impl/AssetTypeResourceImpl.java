package com.evotek.asset.presentation.web.rest.impl;

import com.evotek.asset.application.dto.request.AssetTypeCreateRequest;
import com.evotek.asset.application.dto.request.AssetTypeSearchRequest;
import com.evotek.asset.application.dto.request.AssetTypeUpdateRequest;
import com.evotek.asset.application.mapper.AutoMapperQuery;
import com.evotek.asset.application.service.AssetTypeService;
import com.evotek.asset.domain.AssetType;
import com.evotek.asset.domain.query.AssetTypeSearchQuery;
import com.evotek.asset.infrastructure.persistence.entity.AssetTypeEntity;
import com.evotek.asset.infrastructure.persistence.mapper.AssetTypeEntityMapper;
import com.evotek.asset.infrastructure.persistence.repository.AssetTypeEntityRepository;
import com.evotek.asset.infrastructure.persistence.repository.custom.AssetTypeRepositoryCustom;
import com.evotek.asset.presentation.web.rest.AssetTypeResource;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.persistence.support.SqlUtils;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
import java.util.List;

@RestController
public class AssetTypeResourceImpl implements AssetTypeResource {
    private final AssetTypeService assetTypeService;
    private final AutoMapperQuery autoMapperQuery;

    AssetTypeResourceImpl(AssetTypeService assetTypeService,
                          AutoMapperQuery autoMapperQuery) {
        this.assetTypeService = assetTypeService;
        this.autoMapperQuery = autoMapperQuery;
    }

    @Override
    public Response<AssetType> create(AssetTypeCreateRequest request) {
        AssetType assetType = this.assetTypeService.createAssetType(request);
        return Response.of(assetType);
    }

    @Override
    public Response<AssetType> update(String id, AssetTypeUpdateRequest request) {
        AssetType assetType = this.assetTypeService.updateAssetType(id, request);
        return Response.of(assetType);
    }

    @Override
    public Response<AssetType> getAssetTypeById(String id) {
        AssetType assetType = this.assetTypeService.findAssetById(id);
        return Response.of(assetType);
    }

    @Override
    public Response<Boolean> active(String id) {
        this.assetTypeService.active(id);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public Response<Boolean> inactive(String id) {
        this.assetTypeService.inactive(id);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public Response<Boolean> delete(String id) {
        this.assetTypeService.deleteAssetType(id);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public PagingResponse<AssetType> search(AssetTypeSearchRequest request) {
        List<AssetType> assetTypes = assetTypeService.search(request);
        return new PagingResponse<>(assetTypes, request.getPageIndex(), request.getPageSize(),assetTypeService.count(request));
    }

    @Override
    public PagingResponse<AssetType> autoComplete(AssetTypeSearchRequest request) {
        List<AssetType> assetTypes = assetTypeService.autoComplete(request);
        return new PagingResponse<>(assetTypes, request.getPageIndex(), request.getPageSize(), assetTypeService.count(request));

    }

}
