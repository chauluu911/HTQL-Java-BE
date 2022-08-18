package com.evotek.asset.infrastructure.persistence.repository;

import com.evotek.asset.infrastructure.persistence.entity.AssetHistoryEntity;
import com.evotek.asset.infrastructure.persistence.repository.custom.AssetTypeRepositoryCustom;
import feign.Param;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface AssetHistoryEntityRepository extends JpaRepository<AssetHistoryEntity, String> {
    @Query("SELECT ahe FROM AssetHistoryEntity ahe where ahe.assetId = :id and ahe.deleted = false ")
    public List<AssetHistoryEntity> getAllAssetHistoryByAssetId(@Param("id") String id);
}
