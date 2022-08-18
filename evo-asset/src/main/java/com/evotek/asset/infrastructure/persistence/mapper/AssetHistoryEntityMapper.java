package com.evotek.asset.infrastructure.persistence.mapper;

import com.evotek.asset.domain.AssetHistory;
import com.evotek.asset.infrastructure.persistence.entity.AssetHistoryEntity;
import com.evotek.common.mapper.EntityMapper;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface AssetHistoryEntityMapper extends EntityMapper<AssetHistory, AssetHistoryEntity> {
}
