package com.evotek.meet.application.mapper;

import com.evotek.common.dto.response.iam.UserDTO;
import com.evotek.meet.application.dto.request.MeetingCreateOrUpdateRequest;
import com.evotek.meet.application.dto.request.RoomCreateRequest;
import com.evotek.meet.application.dto.request.RoomUpdateRequest;
import com.evotek.meet.application.dto.response.RoomSchedulerResponse;
import com.evotek.meet.application.dto.response.UserSchedulerResponse;
import com.evotek.meet.domain.RoomScheduler;
import com.evotek.meet.domain.User;
import com.evotek.meet.domain.UserScheduler;
import com.evotek.meet.domain.command.MeetingCreateOrUpdateCmd;
import com.evotek.meet.domain.command.RoomCreateCmd;
import com.evotek.meet.domain.command.RoomUpdateCmd;
import com.evotek.meet.domain.command.UserCreateOrUpdateSchedulerCmd;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface AutoMapper {

    RoomCreateCmd from(RoomCreateRequest request);

    RoomUpdateCmd from(RoomUpdateRequest request);

    UserCreateOrUpdateSchedulerCmd from(MeetingCreateOrUpdateCmd cmd);

    MeetingCreateOrUpdateCmd from(MeetingCreateOrUpdateRequest request);

    User from(UserDTO userDTO);

    List<UserSchedulerResponse> from(List<UserScheduler> userSchedulers);

    UserSchedulerResponse from(UserScheduler userScheduler);

    List<RoomSchedulerResponse> map(List<RoomScheduler> roomSchedulers);
}
