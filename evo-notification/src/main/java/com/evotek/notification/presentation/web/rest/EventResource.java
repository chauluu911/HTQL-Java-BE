package com.evotek.notification.presentation.web.rest;

import com.evotek.common.dto.request.notification.IssueEventRequest;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.notification.application.dto.request.EventCreateRequest;
import com.evotek.notification.application.dto.request.EventSearchRequest;
import com.evotek.notification.application.dto.request.EventUpdateRequest;
import com.evotek.notification.domain.Event;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@Api(tags = "Event Resource")
@RequestMapping("/api")
@Validated
public interface EventResource {

    @ApiOperation("Create event")
    @PreAuthorize("hasPermission(null, 'notification:create')")
    @PostMapping("/events")
    Response<Event> create(@RequestBody @Valid EventCreateRequest request);

    @ApiOperation("Get event")
    @PreAuthorize("hasPermission(null, 'notification:view')")
    @GetMapping("/events/{id}")
    Response<Event> getById(@PathVariable String id);

    @ApiOperation("Update Event")
    @PreAuthorize("hasPermission(null, 'notification:update')")
    @PostMapping("/events/{id}/update")
    Response<Event> update(@PathVariable String id, @RequestBody @Valid EventUpdateRequest request);

    @ApiOperation("Search event")
    @PreAuthorize("hasPermission(null, 'notification:view')")
    @GetMapping("/events")
    PagingResponse<Event> search(@Valid @ValidatePaging(allowedSorts = {"createdAt", "lastModifiedAt"}) EventSearchRequest searchRequest);

    @ApiOperation("Cancel event")
    @PreAuthorize("hasPermission(null, 'notification:update')")
    @PostMapping("/events/{id}/cancel")
    Response<Boolean> cancel(@PathVariable String id);

    @ApiOperation("Delete event")
    @PreAuthorize("hasPermission(null, 'notification:delete')")
    @PostMapping("/events/{id}/delete")
    Response<Boolean> delete(@PathVariable String id);

    @ApiOperation("Send event")
    @PreAuthorize("hasPermission(null, 'notification:update')")
    @PostMapping("/events/{id}/send")
    Response<Void> send(@PathVariable String id);

    @ApiOperation("Create event by system")
    @PreAuthorize("hasPermission(null, 'event:create')")
    @PostMapping("/events/issue")
    Response<Event> issued(@RequestBody @Valid IssueEventRequest request);

}
