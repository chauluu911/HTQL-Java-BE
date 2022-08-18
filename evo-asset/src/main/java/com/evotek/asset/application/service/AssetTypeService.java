package com.evotek.asset.application.service;

import com.evotek.asset.application.dto.request.AssetTypeCreateRequest;
import com.evotek.asset.application.dto.request.AssetTypeSearchRequest;
import com.evotek.asset.application.dto.request.AssetTypeUpdateRequest;
import com.evotek.asset.domain.AssetType;

import java.util.List;

public interface AssetTypeService {

    public AssetType createAssetType(AssetTypeCreateRequest request);

    public void deleteAssetType(String id);

    public AssetType updateAssetType(String id, AssetTypeUpdateRequest request);

    public void active(String id);

    public void inactive(String id);

    public AssetType findAssetById(String id);

    public List<AssetType> search(AssetTypeSearchRequest request);

    public Long count(AssetTypeSearchRequest request);

    public List<AssetType> autoComplete(AssetTypeSearchRequest request);

    public AssetType ensureAssetTypeExisted(String id);
}
