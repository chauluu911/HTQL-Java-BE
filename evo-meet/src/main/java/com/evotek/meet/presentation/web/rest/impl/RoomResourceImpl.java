package com.evotek.meet.presentation.web.rest.impl;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.meet.application.dto.request.RoomCreateRequest;
import com.evotek.meet.application.dto.request.RoomSchedulerRequest;
import com.evotek.meet.application.dto.request.RoomSearchRequest;
import com.evotek.meet.application.dto.request.RoomUpdateRequest;
import com.evotek.meet.application.dto.response.RoomSchedulerResponse;
import com.evotek.meet.application.service.RoomService;
import com.evotek.meet.domain.Room;
import com.evotek.meet.presentation.web.rest.RoomResource;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
public class RoomResourceImpl implements RoomResource {

    private final RoomService roomService;

    public RoomResourceImpl(RoomService roomService) {
        this.roomService = roomService;
    }

    @Override
    public Response<Room> create(RoomCreateRequest request) {
        Room room = roomService.create(request);
        return Response.of(room);
    }

    @Override
    public Response<Room> update(String id, RoomUpdateRequest request) {
        Room room = roomService.update(id, request);
        return Response.of(room);
    }

    @Override
    public Response<Boolean> active(String roomId) {
        roomService.active(roomId);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public Response<Boolean> inactive(String roomId) {
        roomService.inactive(roomId);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public PagingResponse<Room> search(RoomSearchRequest searchRequest) {
        return PagingResponse.of(roomService.searchRoomByRequest(searchRequest));
    }

    @Override
    public PagingResponse<Room> autocomplete(RoomSearchRequest searchRequest) {
        return PagingResponse.of(roomService.autoComplete(searchRequest));
    }

    @Override
    public Response<Room> findByRoomId(String roomId) {
        Room room = roomService.findByRoomId(roomId);
        return Response.of(room);
    }

    @Override
    public Response<List<RoomSchedulerResponse>> getRoomScheduler(String roomId, RoomSchedulerRequest request) {
        List<RoomSchedulerResponse> roomSchedulerResponses = roomService.findSchedulerByRequest(roomId, request);
        return Response.of(roomSchedulerResponses);
    }
}
