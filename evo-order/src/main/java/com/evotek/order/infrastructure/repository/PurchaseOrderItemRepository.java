package com.evotek.order.infrastructure.repository;

import com.evotek.order.infrastructure.entity.PurchaseOrderItemEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PurchaseOrderItemRepository extends JpaRepository<PurchaseOrderItemEntity, String> {

    @Query("select poi from PurchaseOrderItemEntity poi where poi.purchaseOrderId = :orderId and poi.deleted = false ")
    List<PurchaseOrderItemEntity> findAllByOrderId(String orderId);
}
