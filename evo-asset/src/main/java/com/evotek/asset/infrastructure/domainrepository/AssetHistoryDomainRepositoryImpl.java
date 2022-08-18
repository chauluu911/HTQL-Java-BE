package com.evotek.asset.infrastructure.domainrepository;


import com.evotek.asset.domain.AssetHistory;
import com.evotek.asset.domain.repository.AssetHistoryDomainRepository;
import com.evotek.asset.infrastructure.persistence.entity.AssetHistoryEntity;
import com.evotek.asset.infrastructure.persistence.mapper.AssetHistoryEntityMapper;
import com.evotek.asset.infrastructure.persistence.repository.AssetHistoryEntityRepository;
import com.evotek.asset.infrastructure.support.exception.BadRequestError;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class AssetHistoryDomainRepositoryImpl extends AbstractDomainRepository<AssetHistory, AssetHistoryEntity, String> implements AssetHistoryDomainRepository {

    private final AssetHistoryEntityRepository assetHistoryRepository;

    private final AssetHistoryEntityMapper assetHistoryEntityMapper;

    public AssetHistoryDomainRepositoryImpl(AssetHistoryEntityRepository repository, AssetHistoryEntityMapper assetHistoryEntityMapper) {
        super(repository, assetHistoryEntityMapper);
        this.assetHistoryRepository = repository;
        this.assetHistoryEntityMapper = assetHistoryEntityMapper;
    }

    @Override
    public AssetHistory getById(String id) {
        AssetHistoryEntity assetHistoryEntity = this.assetHistoryRepository.findById(id).orElseThrow(
                () -> new ResponseException(BadRequestError.ASSET_HISTORY_NOT_EXIST));
        return assetHistoryEntityMapper.toDomain(assetHistoryEntity);

    }

    @Override
    public List<AssetHistory> getAllByAssetId(String id) {
        List<AssetHistoryEntity> assetHistoryEntities = assetHistoryRepository.getAllAssetHistoryByAssetId(id);
        return assetHistoryEntityMapper.toDomain(assetHistoryEntities);
    }
}
