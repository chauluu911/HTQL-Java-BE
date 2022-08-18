package com.evotek.order.application.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.order.domain.PurchaseOrderItem;
import com.evotek.order.infrastructure.entity.PurchaseOrderItemEntity;
import org.mapstruct.Mapper;
import org.springframework.stereotype.Component;

@Component
@Mapper(componentModel = "spring")
public interface PurchaseOrderItemEntityMapper extends EntityMapper<PurchaseOrderItem, PurchaseOrderItemEntity> {
}
