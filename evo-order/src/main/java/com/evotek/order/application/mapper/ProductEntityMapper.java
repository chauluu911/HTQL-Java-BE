package com.evotek.order.application.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.order.domain.Product;
import com.evotek.order.infrastructure.entity.ProductEntity;
import org.mapstruct.Mapper;
import org.springframework.stereotype.Component;

@Component
@Mapper(componentModel = "spring")
public interface ProductEntityMapper extends EntityMapper<Product, ProductEntity> {
}
