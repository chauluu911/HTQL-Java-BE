package com.evotek.meet.presentation.web.rest.impl;

import com.evotek.common.dto.response.Response;
import com.evotek.meet.application.dto.request.UserSchedulerRequest;
import com.evotek.meet.application.dto.response.UserSchedulerResponse;
import com.evotek.meet.application.service.UserService;
import com.evotek.meet.presentation.web.rest.SchedulerResource;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
public class SchedulerResourceImpl implements SchedulerResource {

    private final UserService userService;

    public SchedulerResourceImpl(
            UserService userService) {
        this.userService = userService;
    }

    @Override
    public Response<List<UserSchedulerResponse>> getScheduler(UserSchedulerRequest request) {
        List<UserSchedulerResponse> userSchedulerResponses = userService.getScheduler(request);
        return Response.of(userSchedulerResponses);
    }

    @Override
    public Response<Void> rejectUserScheduler(String userSchedulerId) {
        userService.reject(userSchedulerId);
        return Response.ok();
    }
}
