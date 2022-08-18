package com.evotek.asset.infrastructure.domainrepository;

import com.evotek.asset.domain.Asset;
import com.evotek.asset.domain.repository.AssetDomainRepository;
import com.evotek.asset.infrastructure.persistence.entity.AssetEntity;
import com.evotek.asset.infrastructure.persistence.mapper.AssetEntityMapper;
import com.evotek.asset.infrastructure.persistence.repository.AssetEntityRepository;
import com.evotek.asset.infrastructure.support.exception.BadRequestError;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import org.springframework.stereotype.Service;

@Service
public class AssetDomainRepositoryImpl extends AbstractDomainRepository<Asset, AssetEntity, String> implements AssetDomainRepository {
    private final AssetEntityRepository assetEntityRepository;

    private final AssetEntityMapper assetEntityMapper;

    protected AssetDomainRepositoryImpl(AssetEntityRepository assetEntityRepository, AssetEntityMapper assetEntityMapper) {
        super(assetEntityRepository, assetEntityMapper);
        this.assetEntityRepository = assetEntityRepository;
        this.assetEntityMapper = assetEntityMapper;
    }

    @Override
    public Asset getById(String id) {
        AssetEntity assetEntity = this.assetEntityRepository.findAssetById(id).orElseThrow(
                () -> new ResponseException(BadRequestError.ASSET_NOT_EXIST));
        return assetEntityMapper.toDomain(assetEntity);
    }
}
