package com.evotek.asset.infrastructure.persistence.mapper;

import com.evotek.asset.domain.AssetType;
import com.evotek.asset.infrastructure.persistence.entity.AssetTypeEntity;
import com.evotek.common.mapper.EntityMapper;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface AssetTypeEntityMapper extends EntityMapper<AssetType, AssetTypeEntity> {
}
