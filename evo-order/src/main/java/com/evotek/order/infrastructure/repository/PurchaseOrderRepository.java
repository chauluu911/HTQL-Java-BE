package com.evotek.order.infrastructure.repository;

import com.evotek.order.infrastructure.entity.PurchaseOrderEntity;
import com.evotek.order.infrastructure.repository.custom.PurchaseOrderRepositoryCustom;
import org.springframework.data.jpa.repository.JpaRepository;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface PurchaseOrderRepository extends JpaRepository<PurchaseOrderEntity, String>, PurchaseOrderRepositoryCustom {

    @Query("select po from PurchaseOrderEntity po where po.menuId = :menuId and po.deleted = false ")
    List<PurchaseOrderEntity> findByMenuId(String menuId);

    @NotNull
    @Query("select po from PurchaseOrderEntity po where po.id = :id and po.deleted = false ")
    Optional<PurchaseOrderEntity> findById(@NotNull String id);

    @Query("select po from PurchaseOrderEntity po where po.id in :ids and po.deleted = false ")
    List<PurchaseOrderEntity> findByIds(List<String> ids);
}
