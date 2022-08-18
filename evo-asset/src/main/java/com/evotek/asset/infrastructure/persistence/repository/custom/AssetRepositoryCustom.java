package com.evotek.asset.infrastructure.persistence.repository.custom;

import com.evotek.asset.domain.query.AssetSearchQuery;
import com.evotek.asset.infrastructure.persistence.entity.AssetEntity;
import org.springframework.data.domain.Page;

import java.util.List;

public interface AssetRepositoryCustom {
    public List<AssetEntity> search(AssetSearchQuery query);

    public Long count(AssetSearchQuery query);

    public List<AssetEntity> searchAutoComplete(AssetSearchQuery query);

}
