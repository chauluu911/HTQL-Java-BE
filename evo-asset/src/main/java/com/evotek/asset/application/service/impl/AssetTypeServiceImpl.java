package com.evotek.asset.application.service.impl;

import com.evotek.asset.application.dto.request.AssetTypeCreateRequest;
import com.evotek.asset.application.dto.request.AssetTypeSearchRequest;
import com.evotek.asset.application.dto.request.AssetTypeUpdateRequest;
import com.evotek.asset.application.mapper.AutoMapper;
import com.evotek.asset.application.mapper.AutoMapperQuery;
import com.evotek.asset.application.service.AssetTypeService;
import com.evotek.asset.domain.AssetType;
import com.evotek.asset.domain.command.AssetTypeCreateCmd;
import com.evotek.asset.domain.command.AssetTypeUpdateCmd;
import com.evotek.asset.domain.query.AssetTypeSearchQuery;
import com.evotek.asset.domain.repository.AssetTypeDomainRepository;
import com.evotek.asset.infrastructure.persistence.entity.AssetTypeEntity;
import com.evotek.asset.infrastructure.persistence.mapper.AssetTypeEntityMapper;
import com.evotek.asset.infrastructure.persistence.repository.AssetTypeEntityRepository;
import com.evotek.asset.infrastructure.persistence.repository.custom.AssetTypeRepositoryCustom;
import com.evotek.asset.infrastructure.support.exception.BadRequestError;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainService;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class AssetTypeServiceImpl extends AbstractDomainService<AssetType, AssetTypeEntity, String> implements AssetTypeService {

    private final AssetTypeEntityRepository assetTypeEntityRepository;
    private final AssetTypeEntityMapper assetTypeEntityMapper;

    private final AssetTypeDomainRepository assetTypeDomainRepository;
    private final AutoMapper autoMapper;
    private final AutoMapperQuery autoMapperQuery;

    protected AssetTypeServiceImpl(AssetTypeEntityRepository assetTypeEntityRepository,
                                   AssetTypeEntityMapper assetTypeEntityMapper,
                                   AutoMapper autoMapper,
                                   AssetTypeDomainRepository assetTypeDomainRepository,
                                   AutoMapperQuery autoMapperQuery) {
        super(assetTypeEntityRepository, assetTypeEntityMapper);
        this.assetTypeEntityRepository = assetTypeEntityRepository;
        this.assetTypeEntityMapper = assetTypeEntityMapper;
        this.autoMapper = autoMapper;
        this.assetTypeDomainRepository = assetTypeDomainRepository;
        this.autoMapperQuery = autoMapperQuery;
    }

    @Override
    public AssetType createAssetType(AssetTypeCreateRequest request) {
        Optional<AssetTypeEntity> assetTypeEntity = this.assetTypeEntityRepository.findAssetTypeByCode(request.getCode());
        if (assetTypeEntity.isPresent()) {
            throw new ResponseException(BadRequestError.ASSET_TYPE_CODE_ALREADY_EXISTED);
        }

        AssetTypeCreateCmd cmd = autoMapper.from(request);
        AssetType assetType = new AssetType(cmd);
        save(assetType);
        return assetType;
    }

    @Override
    public void deleteAssetType(String id) {
        AssetType assetType = ensureAssetTypeExisted(id);
        assetType.delete();
        save(assetType);

    }

    @Override
    public AssetType updateAssetType(String id, AssetTypeUpdateRequest request) {
        AssetType assetType = ensureAssetTypeExisted(id);
        AssetTypeUpdateCmd assetTypeUpdateCmd = autoMapper.from(request);
        assetType.updateAssetType(assetTypeUpdateCmd);
        save(assetType);
        return assetType;
    }

    @Override
    public void active(String id) {
        AssetType assetType = ensureAssetTypeExisted(id);
        assetType.active();
        save(assetType);
    }

    @Override
    public void inactive(String id) {
        AssetType assetType = ensureAssetTypeExisted(id);
        assetType.inactive();
        save(assetType);
    }

    @Override
    public AssetType findAssetById(String id) {
        return ensureAssetTypeExisted(id);
    }

    @Override
    public List<AssetType> search(AssetTypeSearchRequest request) {
        AssetTypeSearchQuery query = autoMapperQuery.from(request);
        List<AssetTypeEntity> assetTypeEntities = assetTypeEntityRepository.search(query);
        return assetTypeEntityMapper.toDomain(assetTypeEntities);
    }

    @Override
    public Long count(AssetTypeSearchRequest request) {
        AssetTypeSearchQuery query = autoMapperQuery.from(request);
        return assetTypeEntityRepository.count(query);
    }

    @Override
    public List<AssetType> autoComplete(AssetTypeSearchRequest request) {
        AssetTypeSearchQuery query = autoMapperQuery.from(request);
        List<AssetTypeEntity> assetTypeEntities = assetTypeEntityRepository.searchAutoComplete(query);
        return assetTypeEntityMapper.toDomain(assetTypeEntities);
    }

    @Override
    public AssetType ensureAssetTypeExisted(String id) {
        return assetTypeDomainRepository.getById(id);
    }


}
