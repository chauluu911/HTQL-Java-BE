package com.evotek.order.application.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.order.domain.PurchaseOrderHistory;
import com.evotek.order.infrastructure.entity.PurchaseOrderHistoryEntity;
import org.mapstruct.Mapper;
import org.springframework.stereotype.Component;

@Component
@Mapper(componentModel = "spring")
public interface PurchaseOrderHistoryEntityMapper extends EntityMapper<PurchaseOrderHistory, PurchaseOrderHistoryEntity> {
}
