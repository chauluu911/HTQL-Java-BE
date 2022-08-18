package com.evotek.meet.presentation.web.rest;

import com.evotek.common.dto.response.Response;
import com.evotek.meet.application.dto.request.UserSchedulerRequest;
import com.evotek.meet.application.dto.response.UserSchedulerResponse;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.List;

@Api(tags = "Scheduler Resource")
@RequestMapping("/api")
@Validated
public interface SchedulerResource {

    @ApiOperation(value = "Get scheduler of me")
    @GetMapping("/me/schedulers")
    @PreAuthorize("hasPermission(null, 'meeting:view')")
    Response<List<UserSchedulerResponse>> getScheduler(UserSchedulerRequest request);

    @ApiOperation(value = "Reject scheduler")
    @PostMapping("/me/schedulers/{userSchedulerId}/reject")
    @PreAuthorize("hasPermission(null, 'meeting:view')")
    Response<Void> rejectUserScheduler(@PathVariable String userSchedulerId);
}
