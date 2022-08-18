package com.evotek.asset.application.mapper;

import com.evotek.asset.application.dto.request.AssetSearchRequest;
import com.evotek.asset.application.dto.request.AssetTypeSearchRequest;
import com.evotek.asset.domain.query.AssetSearchQuery;
import com.evotek.asset.domain.query.AssetTypeSearchQuery;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface AutoMapperQuery {
    AssetTypeSearchQuery from(AssetTypeSearchRequest request);

    AssetSearchQuery from(AssetSearchRequest request);
}
