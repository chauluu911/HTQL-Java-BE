package com.evotek.asset.infrastructure.persistence.mapper;

import com.evotek.asset.domain.Asset;
import com.evotek.asset.infrastructure.persistence.entity.AssetEntity;
import com.evotek.common.mapper.EntityMapper;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface AssetEntityMapper extends EntityMapper<Asset, AssetEntity> {
}
