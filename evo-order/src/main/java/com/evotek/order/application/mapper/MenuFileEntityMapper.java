package com.evotek.order.application.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.order.domain.MenuFile;
import com.evotek.order.infrastructure.entity.MenuFileEntity;
import org.mapstruct.Mapper;
import org.springframework.stereotype.Component;

@Component
@Mapper(componentModel = "spring")
public interface MenuFileEntityMapper extends EntityMapper<MenuFile, MenuFileEntity> {
}
