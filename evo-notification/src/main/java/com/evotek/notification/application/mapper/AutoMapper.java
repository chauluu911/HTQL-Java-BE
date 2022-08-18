package com.evotek.notification.application.mapper;

import com.evotek.common.dto.request.notification.IssueEventRequest;
import com.evotek.notification.application.dto.request.DeviceCreateRequest;
import com.evotek.notification.application.dto.request.EventCreateRequest;
import com.evotek.notification.application.dto.request.EventUpdateRequest;
import com.evotek.notification.domain.command.DeviceCreateOrUpdateCmd;
import com.evotek.notification.domain.command.EventCreateCommand;
import com.evotek.notification.domain.command.EventUpdateCommand;
import com.evotek.notification.domain.command.IssueEventCmd;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface AutoMapper {

    EventCreateCommand from(EventCreateRequest request);

    EventUpdateCommand from(EventUpdateRequest request);

    IssueEventCmd from(IssueEventRequest request);

    DeviceCreateOrUpdateCmd from(DeviceCreateRequest request);
}
