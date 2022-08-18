package com.evotek.asset.infrastructure.persistence.repository.custom;

import com.evotek.asset.domain.query.AssetSearchQuery;
import com.evotek.asset.domain.query.AssetTypeSearchQuery;
import com.evotek.asset.infrastructure.persistence.entity.AssetTypeEntity;

import java.util.List;

public interface AssetTypeRepositoryCustom {
    List<AssetTypeEntity> search(AssetTypeSearchQuery query);

    Long count(AssetTypeSearchQuery query);

    List<AssetTypeEntity> searchAutoComplete(AssetTypeSearchQuery query);
}
