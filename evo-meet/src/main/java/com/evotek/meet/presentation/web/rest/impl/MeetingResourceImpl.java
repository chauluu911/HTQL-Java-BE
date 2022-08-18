package com.evotek.meet.presentation.web.rest.impl;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.meet.application.dto.request.MeetingCreateOrUpdateRequest;
import com.evotek.meet.application.dto.request.MeetingSearchRequest;
import com.evotek.meet.application.service.MeetingService;
import com.evotek.meet.domain.Meeting;
import com.evotek.meet.presentation.web.rest.MeetingResource;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class MeetingResourceImpl implements MeetingResource {

    private final MeetingService meetingService;

    public MeetingResourceImpl(MeetingService meetingService) {
        this.meetingService = meetingService;
    }

    @Override
    public Response<Meeting> create(MeetingCreateOrUpdateRequest request) {
        Meeting meeting = meetingService.create(request);
        return Response.of(meeting);
    }

    @Override
    public Response<Meeting> update(String id, MeetingCreateOrUpdateRequest request) {
        Meeting meeting = meetingService.update(id, request);
        return Response.of(meeting);
    }

    @Override
    public Response<Meeting> approve(String id) {
        Meeting meeting = meetingService.approve(id);
        return Response.of(meeting);
    }

    @Override
    public Response<Meeting> reject(String id) {
        Meeting meeting = meetingService.reject(id);
        return Response.of(meeting);
    }

    @Override
    public Response<Meeting> delete(String id) {
        meetingService.delete(id);
        return Response.ok();
    }

    @Override
    public Response<Meeting> cancel(String id) {
        meetingService.cancel(id);
        return Response.ok();
    }

    @Override
    public Response<Meeting> findByMeetingId(String id) {
        Meeting meeting = meetingService.findByMeetingId(id);
        return Response.of(meeting);
    }

    @Override
    public PagingResponse<Meeting> search(MeetingSearchRequest searchRequest) {
        return PagingResponse.of(meetingService.search(searchRequest));
    }
}
