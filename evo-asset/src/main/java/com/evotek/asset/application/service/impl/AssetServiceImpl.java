package com.evotek.asset.application.service.impl;

import com.evotek.asset.application.dto.request.AssetCreateRequest;
import com.evotek.asset.application.dto.request.AssetSearchRequest;
import com.evotek.asset.application.dto.request.AssetUpdateRequest;
import com.evotek.asset.application.mapper.AutoMapper;
import com.evotek.asset.application.mapper.AutoMapperQuery;
import com.evotek.asset.application.service.AssetService;
import com.evotek.asset.application.service.AssetTypeService;
import com.evotek.asset.domain.Asset;
import com.evotek.asset.domain.AssetHistory;
import com.evotek.asset.domain.AssetType;
import com.evotek.asset.domain.command.AssetCreateCmd;
import com.evotek.asset.domain.command.AssetUpdateCmd;
import com.evotek.asset.domain.query.AssetSearchQuery;
import com.evotek.asset.domain.repository.AssetDomainRepository;
import com.evotek.asset.domain.repository.AssetHistoryDomainRepository;
import com.evotek.asset.domain.repository.AssetTypeDomainRepository;
import com.evotek.asset.infrastructure.persistence.entity.AssetEntity;
import com.evotek.asset.infrastructure.persistence.mapper.AssetEntityMapper;
import com.evotek.asset.infrastructure.persistence.repository.AssetEntityRepository;
import com.evotek.asset.infrastructure.persistence.repository.custom.AssetRepositoryCustom;
import com.evotek.asset.infrastructure.support.enums.AssetStatus;
import com.evotek.asset.infrastructure.support.exception.BadRequestError;
import com.evotek.common.client.iam.IAMClient;
import com.evotek.common.client.order.OrderClient;
import com.evotek.common.dto.response.iam.UserDTO;
import com.evotek.common.dto.response.order.ProductDTO;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.persistence.support.SeqRepository;
import com.evotek.common.webapp.support.AbstractDomainService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

@Service
public class AssetServiceImpl extends AbstractDomainService<Asset, AssetEntity, String> implements AssetService {
    private final AssetEntityRepository assetEntityRepository;
    private final AssetEntityMapper assetEntityMapper;
    private final AssetTypeDomainRepository assetTypeDomainRepository;
    private final AutoMapper autoMapper;
    private final IAMClient iamClient;
    private final OrderClient orderClient;
    private final SeqRepository seqRepository;
    private final AssetHistoryDomainRepository assetHistoryDomainRepository;
    private final AssetDomainRepository assetDomainRepository;
    private final AutoMapperQuery autoMapperQuery;
    private final AssetTypeService assetTypeService;

    protected AssetServiceImpl(AssetEntityRepository assetEntityRepository,
                               AssetEntityMapper assetEntityMapper,
                               AssetTypeDomainRepository assetTypeDomainRepository,
                               AutoMapper autoMapper,
                               IAMClient iamClient,
                               OrderClient orderClient,
                               SeqRepository seqRepository,
                               AssetHistoryDomainRepository assetHistoryDomainRepository,
                               AssetDomainRepository assetDomainRepository,
                               AutoMapperQuery autoMapperQuery,
                               AssetTypeService assetTypeService) {
        super(assetEntityRepository, assetEntityMapper);
        this.assetEntityRepository = assetEntityRepository;
        this.assetEntityMapper = assetEntityMapper;
        this.assetTypeDomainRepository = assetTypeDomainRepository;
        this.autoMapper = autoMapper;
        this.iamClient = iamClient;
        this.orderClient = orderClient;
        this.seqRepository = seqRepository;
        this.assetHistoryDomainRepository = assetHistoryDomainRepository;
        this.assetDomainRepository = assetDomainRepository;
        this.autoMapperQuery = autoMapperQuery;
        this.assetTypeService = assetTypeService;
    }

    @Transactional
    @Override
    public Asset createAsset(AssetCreateRequest request) {
        AssetCreateCmd cmd = autoMapper.from(request);
        UserDTO userDTO = iamClient.findUserById(cmd.getOwnerId()).getData();
        ProductDTO productDTO = orderClient.findById(cmd.getProductId()).getData();
        AssetType assetType = assetTypeService.ensureAssetTypeExisted(cmd.getAssetTypeId());
        Asset asset = new Asset(cmd);
        asset.enrichAssetType(assetType);
        asset.enrichCode(seqRepository.generateAssetCode(productDTO.getType()));
        AssetHistory assetHistory = new AssetHistory(asset, userDTO.getId());
        assetDomainRepository.save(asset);
        assetHistoryDomainRepository.save(assetHistory);
        return asset;
    }

    @Transactional
    @Override
    public Asset updateAsset(String id, AssetUpdateRequest request) {
        AssetUpdateCmd cmd = autoMapper.from(request);
        UserDTO userDTO = null;
        if (Objects.nonNull(cmd.getOwnerId())) {
            userDTO = iamClient.findUserById(cmd.getOwnerId()).getData();
        }
        String ownerId = Objects.isNull(userDTO) ? null : userDTO.getId();
        AssetType assetType = assetTypeService.ensureAssetTypeExisted(request.getAssetTypeId());
        Asset asset = ensureAssetExisted(id);
        asset.enrichAssetType(assetType);
//        if (Objects.equals(asset.getStatus(), AssetStatus.DESTROY)) {
//            throw new ResponseException(BadRequestError.ASSET_WAS_DESTROYED);
//        } else if (Objects.equals(asset.getStatus(), AssetStatus.LIQUIDATE)) {
//            throw new ResponseException(BadRequestError.ASSET_WAS_LIQUIDATED);
//        }
        asset.updateAsset(cmd);
        AssetHistory assetHistory = new AssetHistory(asset, ownerId);
        assetDomainRepository.save(asset);
        assetHistoryDomainRepository.save(assetHistory);
        return asset;
    }

    @Override
    public void deleteAsset(String id) {
        Asset asset = ensureAssetExisted(id);
        asset.delete();
        assetDomainRepository.save(asset);
    }

    @Override
    public Asset getAssetById(String id) {
        Asset asset = ensureAssetExisted(id);
        List<AssetHistory> assetHistories = assetHistoryDomainRepository.getAllByAssetId(id);
        asset.enrichAssetHistory(assetHistories);
        AssetType assetType = assetTypeService.ensureAssetTypeExisted(asset.getAssetTypeId());
        asset.enrichAssetType(assetType);
        return asset;
    }

    @Override
    public List<Asset> search(AssetSearchRequest request) {
        AssetSearchQuery query = autoMapperQuery.from(request);
        List<AssetEntity> assetEntities = assetEntityRepository.search(query);
        return assetEntityMapper.toDomain(assetEntities);
    }

    @Override
    public List<Asset> searchAutocomplete(AssetSearchRequest request) {
        AssetSearchQuery query = autoMapperQuery.from(request);
        List<AssetEntity> assetEntities = assetEntityRepository.searchAutoComplete(query);
        return assetEntityMapper.toDomain(assetEntities);
    }

    @Override
    public Long count(AssetSearchRequest request) {
        AssetSearchQuery query = autoMapperQuery.from(request);
        return assetEntityRepository.count(query);
    }

    @Override
    public Asset ensureAssetExisted(String id) {
        return assetDomainRepository.getById(id);
    }
}
