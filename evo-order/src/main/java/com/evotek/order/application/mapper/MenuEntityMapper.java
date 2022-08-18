package com.evotek.order.application.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.order.domain.Menu;
import com.evotek.order.infrastructure.entity.MenuEntity;
import org.mapstruct.Mapper;
import org.springframework.stereotype.Component;

@Component
@Mapper(componentModel = "spring")
public interface MenuEntityMapper extends EntityMapper<Menu, MenuEntity> {
}
