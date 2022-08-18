package com.evotek.asset.presentation.web.rest.impl;

import com.evotek.asset.application.dto.request.AssetCreateRequest;
import com.evotek.asset.application.dto.request.AssetSearchRequest;
import com.evotek.asset.application.dto.request.AssetUpdateRequest;
import com.evotek.asset.application.mapper.AutoMapperQuery;
import com.evotek.asset.application.service.AssetService;
import com.evotek.asset.application.service.AssetTypeService;
import com.evotek.asset.domain.Asset;
import com.evotek.asset.domain.AssetType;
import com.evotek.asset.presentation.web.rest.AssetResource;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
public class AssetResourceImpl implements AssetResource {
    private final AssetService assetService;
    private final AutoMapperQuery autoMapperQuery;
    private final AssetTypeService assetTypeService;

    public AssetResourceImpl(AssetService assetService,
                             AutoMapperQuery autoMapperQuery, AssetTypeService assetTypeService) {
        this.assetService = assetService;
        this.autoMapperQuery = autoMapperQuery;
        this.assetTypeService = assetTypeService;
    }

    @Override
    public Response<Asset> create(AssetCreateRequest request) {
        Asset asset = assetService.createAsset(request);
        return Response.of(asset);
    }

    @Override
    public Response<Asset> update(String id, AssetUpdateRequest request) {
        Asset asset = assetService.updateAsset(id, request);
        return Response.of(asset);
    }

    @Override
    public Response<Asset> getAssetById(String id) {
        Asset asset = assetService.getAssetById(id);
        return Response.of(asset);
    }

    @Override
    public PagingResponse<Asset> search(AssetSearchRequest request) {
        List<Asset> assets = assetService.search(request);
        for (Asset asset : assets) {
            AssetType assetType = assetTypeService.ensureAssetTypeExisted(asset.getAssetTypeId());
            asset.enrichAssetType(assetType);
        }
        return new PagingResponse<>(assets, request.getPageIndex(), request.getPageSize(), assetService.count(request));
    }

    @Override
    public PagingResponse<Asset> autoComplete(AssetSearchRequest request) {
        List<Asset> assets = assetService.searchAutocomplete(request);
        return new PagingResponse<>(assets, request.getPageIndex(), request.getPageSize(), assetService.count(request));
    }
}
