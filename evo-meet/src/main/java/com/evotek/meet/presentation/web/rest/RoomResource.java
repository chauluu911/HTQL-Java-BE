package com.evotek.meet.presentation.web.rest;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.meet.application.dto.request.RoomCreateRequest;
import com.evotek.meet.application.dto.request.RoomSchedulerRequest;
import com.evotek.meet.application.dto.request.RoomSearchRequest;
import com.evotek.meet.application.dto.request.RoomUpdateRequest;
import com.evotek.meet.application.dto.response.RoomSchedulerResponse;
import com.evotek.meet.domain.Room;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

@Api(tags = "Room Resource")
@RequestMapping("/api")
@Validated
public interface RoomResource {

    @ApiOperation(value = "Create room")
    @PostMapping("/rooms")
    @PreAuthorize("hasPermission(null, 'room:create')")
    Response<Room> create(@RequestBody @Valid RoomCreateRequest request);

    @ApiOperation(value = "Update room")
    @PostMapping("/rooms/{id}/update")
    @PreAuthorize("hasPermission(null, 'room:update')")
    Response<Room> update(@PathVariable String id, @RequestBody @Valid RoomUpdateRequest request);

    @ApiOperation(value = "Active Room")
    @PostMapping("/rooms/{roomId}/active")
    @PreAuthorize("hasPermission(null, 'room:update')")
    Response<Boolean> active(@PathVariable String roomId);

    @ApiOperation(value = "Inactive Room")
    @PostMapping("/rooms/{roomId}/inactive")
    @PreAuthorize("hasPermission(null, 'room:update')")
    Response<Boolean> inactive(@PathVariable String roomId);

    @ApiOperation(value = "Search rooms")
    @GetMapping("/rooms")
    @PreAuthorize("hasPermission(null, 'room:view')")
    PagingResponse<Room> search(@ValidatePaging(allowedSorts = {"id", "code", "name", "createdAt", "createdBy", "lastModifiedAt", "status"})
                                        RoomSearchRequest searchRequest);

    @ApiOperation(value = "Search rooms auto complete")
    @GetMapping("/rooms/auto-complete")
    @PreAuthorize("hasPermission(null, 'room:view')")
    PagingResponse<Room> autocomplete(@ValidatePaging(allowedSorts = {"id", "code", "name", "createdAt", "createdBy", "lastModifiedAt", "status"})
                                              RoomSearchRequest searchRequest);

    @ApiOperation(value = "Find room")
    @GetMapping("/rooms/{roomId}")
    @PreAuthorize("hasPermission(null, 'room:view')")
    Response<Room> findByRoomId(@PathVariable String roomId);

    @ApiOperation(value = "get schedulers of room")
    @GetMapping("/rooms/room-schedulers/{roomId}")
    @PreAuthorize("hasPermission(null, 'room:view')")
    Response<List<RoomSchedulerResponse>> getRoomScheduler(@PathVariable String roomId, RoomSchedulerRequest request);
}
