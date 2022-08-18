package com.evotek.order.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.order.application.mapper.PurchaseOrderEntityMapper;
import com.evotek.order.application.mapper.PurchaseOrderHistoryEntityMapper;
import com.evotek.order.application.mapper.PurchaseOrderItemEntityMapper;
import com.evotek.order.domain.PurchaseOrder;
import com.evotek.order.domain.repository.PurchaseOrderDomainRepository;
import com.evotek.order.infrastructure.entity.PurchaseOrderEntity;
import com.evotek.order.infrastructure.entity.PurchaseOrderHistoryEntity;
import com.evotek.order.infrastructure.entity.PurchaseOrderItemEntity;
import com.evotek.order.infrastructure.repository.PurchaseOrderHistoryRepository;
import com.evotek.order.infrastructure.repository.PurchaseOrderItemRepository;
import com.evotek.order.infrastructure.repository.PurchaseOrderRepository;
import com.evotek.order.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

@Slf4j
@Repository
public class PurchaseOrderDomainRepositoryImpl extends AbstractDomainRepository<PurchaseOrder, PurchaseOrderEntity, String>
        implements PurchaseOrderDomainRepository {

    private final PurchaseOrderRepository purchaseOrderRepository;
    private final PurchaseOrderEntityMapper purchaseOrderEntityMapper;
    private final PurchaseOrderHistoryRepository purchaseOrderHistoryRepository;
    private final PurchaseOrderHistoryEntityMapper purchaseOrderHistoryEntityMapper;
    private final PurchaseOrderItemRepository purchaseOrderItemRepository;
    private final PurchaseOrderItemEntityMapper purchaseOrderItemEntityMapper;


    public PurchaseOrderDomainRepositoryImpl(
            PurchaseOrderRepository purchaseOrderRepository, PurchaseOrderEntityMapper purchaseOrderEntityMapper,
            PurchaseOrderHistoryRepository purchaseOrderHistoryRepository, PurchaseOrderHistoryEntityMapper purchaseOrderHistoryEntityMapper,
            PurchaseOrderItemRepository purchaseOrderItemRepository, PurchaseOrderItemEntityMapper purchaseOrderItemEntityMapper
    ) {
        super(purchaseOrderRepository, purchaseOrderEntityMapper);
        this.purchaseOrderRepository = purchaseOrderRepository;
        this.purchaseOrderEntityMapper = purchaseOrderEntityMapper;
        this.purchaseOrderHistoryRepository = purchaseOrderHistoryRepository;
        this.purchaseOrderHistoryEntityMapper = purchaseOrderHistoryEntityMapper;
        this.purchaseOrderItemRepository = purchaseOrderItemRepository;
        this.purchaseOrderItemEntityMapper = purchaseOrderItemEntityMapper;
    }

    @Override
    public PurchaseOrder getById(String id) {
        return this.findById(id).orElseThrow(() -> new ResponseException(NotFoundError.PURCHASE_ORDER_HISTORY_NOT_FOUND));
    }

    @Override
    @Transactional
    public List<PurchaseOrder> saveAll(List<PurchaseOrder> purchaseOrders) {

        if (CollectionUtils.isEmpty(purchaseOrders)) {
            return new ArrayList<>();
        }
        List<PurchaseOrderEntity> purchaseOrderEntities = purchaseOrderEntityMapper.toEntity(purchaseOrders);
        purchaseOrderRepository.saveAll(purchaseOrderEntities);
        List<PurchaseOrderItemEntity> purchaseOrderItemEntities = new ArrayList<>();
        List<PurchaseOrderHistoryEntity> purchaseOrderHistoryEntities = new ArrayList<>();
        for (PurchaseOrder purchaseOrder : purchaseOrders) {
            if (!CollectionUtils.isEmpty(purchaseOrder.getPurchaseOrderItems())) {
                purchaseOrderItemEntities
                        .addAll(purchaseOrderItemEntityMapper.toEntity(purchaseOrder.getPurchaseOrderItems()));
            }
            if (!CollectionUtils.isEmpty(purchaseOrder.getPurchaseOrderHistories())) {
                purchaseOrderHistoryEntities
                        .addAll(purchaseOrderHistoryEntityMapper.toEntity(purchaseOrder.getPurchaseOrderHistories()));
            }
        }
        purchaseOrderItemRepository.saveAll(purchaseOrderItemEntities);
        purchaseOrderHistoryRepository.saveAll(purchaseOrderHistoryEntities);
        return purchaseOrders;
    }
}
