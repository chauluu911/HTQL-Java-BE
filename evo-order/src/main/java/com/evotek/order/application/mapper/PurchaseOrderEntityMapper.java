package com.evotek.order.application.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.order.domain.PurchaseOrder;
import com.evotek.order.infrastructure.entity.PurchaseOrderEntity;
import org.mapstruct.Mapper;
import org.springframework.stereotype.Component;

@Component
@Mapper(componentModel = "spring")
public interface PurchaseOrderEntityMapper extends EntityMapper<PurchaseOrder, PurchaseOrderEntity> {
}
