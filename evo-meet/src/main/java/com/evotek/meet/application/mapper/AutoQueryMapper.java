package com.evotek.meet.application.mapper;

import com.evotek.meet.application.dto.request.MeetingSearchRequest;
import com.evotek.meet.application.dto.request.RoomSearchRequest;
import com.evotek.meet.domain.query.MeetingSearchQuery;
import com.evotek.meet.domain.query.RoomSearchQuery;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface AutoQueryMapper {
    MeetingSearchQuery from(MeetingSearchRequest request);

    RoomSearchQuery from(RoomSearchRequest request);
}
