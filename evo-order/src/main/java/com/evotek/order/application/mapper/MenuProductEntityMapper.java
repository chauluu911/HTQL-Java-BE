package com.evotek.order.application.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.order.domain.MenuProduct;
import com.evotek.order.infrastructure.entity.MenuProductEntity;
import org.mapstruct.Mapper;
import org.springframework.stereotype.Component;

@Component
@Mapper(componentModel = "spring")
public interface MenuProductEntityMapper extends EntityMapper<MenuProduct, MenuProductEntity> {
}
