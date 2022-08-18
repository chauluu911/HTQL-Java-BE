package com.evotek.order.infrastructure.repository;

import com.evotek.order.infrastructure.entity.PurchaseOrderHistoryEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface PurchaseOrderHistoryRepository extends JpaRepository<PurchaseOrderHistoryEntity, String> {

    @Query("select poh from PurchaseOrderHistoryEntity poh where poh.purchaseOrderId = :purchaseOrderId and poh.deleted = false ")
    List<PurchaseOrderHistoryEntity> findAllByPurchaseOrderId(String purchaseOrderId);
}
