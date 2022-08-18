package com.evotek.asset.infrastructure.persistence.repository;

import com.evotek.asset.infrastructure.persistence.entity.AssetTypeEntity;
import com.evotek.asset.infrastructure.persistence.repository.custom.AssetTypeRepositoryCustom;
import com.evotek.asset.infrastructure.support.enums.AssetTypeStatus;
import feign.Param;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface AssetTypeEntityRepository extends JpaRepository<AssetTypeEntity, String>, AssetTypeRepositoryCustom {
    @Query("SELECT ate FROM AssetTypeEntity ate where ate.id = :id and ate.deleted = false")
    Optional<AssetTypeEntity> findAssetTypeById(@Param("id") String id);

    @Query(value = "SELECT ate FROM AssetTypeEntity ate where ate.deleted = false ")
    List<AssetTypeEntity> findAllAssetType();

    @Query(value = "SELECT ate FROM AssetTypeEntity ate where ate.code = :code and ate.deleted = false ")
    Optional<AssetTypeEntity> findAssetTypeByCode( @Param("code") String code);

}
