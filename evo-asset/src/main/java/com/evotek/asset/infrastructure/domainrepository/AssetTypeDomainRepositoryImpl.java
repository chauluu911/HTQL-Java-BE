package com.evotek.asset.infrastructure.domainrepository;

import com.evotek.asset.domain.AssetType;
import com.evotek.asset.domain.repository.AssetTypeDomainRepository;
import com.evotek.asset.infrastructure.persistence.entity.AssetTypeEntity;
import com.evotek.asset.infrastructure.persistence.mapper.AssetTypeEntityMapper;
import com.evotek.asset.infrastructure.persistence.repository.AssetTypeEntityRepository;
import com.evotek.asset.infrastructure.support.exception.BadRequestError;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import org.springframework.stereotype.Service;

@Service
public class AssetTypeDomainRepositoryImpl extends AbstractDomainRepository<AssetType, AssetTypeEntity, String> implements AssetTypeDomainRepository {
    private final AssetTypeEntityRepository assetTypeEntityRepository;
    private final AssetTypeEntityMapper assetTypeEntityMapper;

    protected AssetTypeDomainRepositoryImpl(AssetTypeEntityRepository assetTypeEntityRepository, AssetTypeEntityMapper assetTypeEntityMapper) {
        super(assetTypeEntityRepository, assetTypeEntityMapper);
        this.assetTypeEntityRepository = assetTypeEntityRepository;
        this.assetTypeEntityMapper = assetTypeEntityMapper;
    }

    @Override
    public AssetType getById(String id) {
        AssetTypeEntity assetTypeEntity = this.assetTypeEntityRepository.findAssetTypeById(id).orElseThrow(
                () -> new ResponseException(BadRequestError.ASSET_TYPE_NOT_EXIST)
        );
        return assetTypeEntityMapper.toDomain(assetTypeEntity);
    }
}
