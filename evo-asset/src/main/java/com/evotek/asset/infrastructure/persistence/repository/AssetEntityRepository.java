package com.evotek.asset.infrastructure.persistence.repository;

import com.evotek.asset.infrastructure.persistence.entity.AssetEntity;
import com.evotek.asset.infrastructure.persistence.repository.custom.AssetRepositoryCustom;
import feign.Param;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface AssetEntityRepository extends JpaRepository<AssetEntity, String>, AssetRepositoryCustom {
    @Query("SELECT ae FROM AssetEntity ae where ae.id = :id and ae.deleted = false")
    Optional<AssetEntity> findAssetById(@Param("id") String id);
}
