package com.evotek.meet.application.service;


import com.evotek.common.dto.PageDTO;
import com.evotek.common.webapp.support.DomainService;
import com.evotek.meet.application.dto.request.RoomCreateRequest;
import com.evotek.meet.application.dto.request.RoomSchedulerRequest;
import com.evotek.meet.application.dto.request.RoomSearchRequest;
import com.evotek.meet.application.dto.request.RoomUpdateRequest;
import com.evotek.meet.application.dto.response.RoomSchedulerResponse;
import com.evotek.meet.domain.Room;

import java.util.List;

public interface RoomService extends DomainService<Room, String> {

    Room create(RoomCreateRequest request);

    Room update(String id, RoomUpdateRequest request);

    void active(String roomId);

    void inactive(String roomId);

    Room ensureExisted(String id);

    PageDTO<Room> searchRoomByRequest(RoomSearchRequest searchRequest);

    PageDTO<Room> autoComplete(RoomSearchRequest request);

    Room findByRoomId(String roomId);

    List<RoomSchedulerResponse> findSchedulerByRequest(String roomId, RoomSchedulerRequest request);
}
