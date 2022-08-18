package com.evotek.asset.application.service;

import com.evotek.asset.application.dto.request.AssetCreateRequest;
import com.evotek.asset.application.dto.request.AssetSearchRequest;
import com.evotek.asset.application.dto.request.AssetUpdateRequest;
import com.evotek.asset.domain.Asset;
import com.evotek.asset.domain.command.AssetUpdateCmd;

import java.util.List;

public interface AssetService {
    public Asset createAsset(AssetCreateRequest request);

    public Asset updateAsset(String id, AssetUpdateRequest request);

    public void deleteAsset(String id);

    public Asset getAssetById(String id);

    public List<Asset> search(AssetSearchRequest request);

    public List<Asset> searchAutocomplete(AssetSearchRequest request);

    public Long count(AssetSearchRequest request);

    public Asset ensureAssetExisted(String id);
}
