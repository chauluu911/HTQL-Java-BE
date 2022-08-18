package com.evotek.notification.presentation.web.rest.impl;

import com.evotek.common.dto.request.notification.IssueEventRequest;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.notification.application.dto.request.EventCreateRequest;
import com.evotek.notification.application.dto.request.EventSearchRequest;
import com.evotek.notification.application.dto.request.EventUpdateRequest;
import com.evotek.notification.application.service.EventService;
import com.evotek.notification.domain.Event;
import com.evotek.notification.presentation.web.rest.EventResource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@Slf4j
public class EventResourceImpl implements EventResource {

    private final EventService eventService;

    public EventResourceImpl(EventService eventService) {
        this.eventService = eventService;
    }

    @Override
    public Response<Event> create(@Valid EventCreateRequest request) {
        Event event = eventService.create(request);
        return Response.of(event);
    }

    @Override
    public Response<Event> getById(String id) {
        Event event = eventService.getById(id);
        return Response.of(event);
    }

    @Override
    public Response<Event> update(String id, @Valid EventUpdateRequest request) {
        Event event = eventService.update(id, request);
        return Response.of(event);
    }

    @Override
    public PagingResponse<Event> search(EventSearchRequest searchRequest) {
        return PagingResponse.of(eventService.search(searchRequest));
    }

    @Override
    public Response<Boolean> cancel(String id) {
        eventService.cancel(id);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public Response<Boolean> delete(String id) {
        eventService.delete(id);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public Response<Void> send(String id) {
        eventService.send(id);
        return Response.ok();
    }

    @Override
    public Response<Event> issued(IssueEventRequest request) {
        Event event = eventService.issued(request);
        return Response.of(event);
    }

}
