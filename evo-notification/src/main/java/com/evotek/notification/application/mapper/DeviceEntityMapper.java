package com.evotek.notification.application.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.notification.domain.Device;
import com.evotek.notification.infrastructure.persistence.entity.DeviceEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface DeviceEntityMapper extends EntityMapper<Device, DeviceEntity> {

}
