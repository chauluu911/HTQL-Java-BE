package com.evotek.order.infrastructure.repository.custom;

import com.evotek.order.infrastructure.entity.PurchaseOrderEntity;
import com.evotek.order.infrastructure.query.PurchaseOrderSearchQuery;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PurchaseOrderRepositoryCustom {
    List<PurchaseOrderEntity> search(PurchaseOrderSearchQuery searchQuery);

    Long countOrder(PurchaseOrderSearchQuery searchQuery);
}
