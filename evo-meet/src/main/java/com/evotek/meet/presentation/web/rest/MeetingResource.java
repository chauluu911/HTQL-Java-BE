package com.evotek.meet.presentation.web.rest;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.meet.application.dto.request.MeetingCreateOrUpdateRequest;
import com.evotek.meet.application.dto.request.MeetingSearchRequest;
import com.evotek.meet.domain.Meeting;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@Api(tags = "Meeting Resource")
@RequestMapping("/api")
@Validated
public interface MeetingResource {

    @ApiOperation(value = "Create meeting")
    @PostMapping("/meets")
    @PreAuthorize("hasPermission(null, 'meeting:create')")
    Response<Meeting> create(@RequestBody @Valid MeetingCreateOrUpdateRequest request);

    @ApiOperation(value = "Update meeting")
    @PostMapping("/meets/{id}/update")
    @PreAuthorize("hasPermission(null, 'meeting:update')")
    Response<Meeting> update(@PathVariable String id, @RequestBody @Valid MeetingCreateOrUpdateRequest request);

    @ApiOperation(value = "Approve meeting")
    @PostMapping("/meets/{id}/approve")
    @PreAuthorize("hasPermission(null, 'meeting:update')")
    Response<Meeting> approve(@PathVariable String id);

    @ApiOperation(value = "Reject meeting")
    @PostMapping("/meets/{id}/reject")
    @PreAuthorize("hasPermission(null, 'meeting:update')")
    Response<Meeting> reject(@PathVariable String id);

    @ApiOperation(value = "Delete meeting")
    @PostMapping("/meets/{id}/delete")
    @PreAuthorize("hasPermission(null, 'meeting:update')")
    Response<Meeting> delete(@PathVariable String id);

    @ApiOperation(value = "Cancel meeting")
    @PostMapping("/meets/{id}/cancel")
    @PreAuthorize("hasPermission(null, 'meeting:update')")
    Response<Meeting> cancel(@PathVariable String id);

    @ApiOperation(value = "find meeting")
    @GetMapping("/meets/{id}")
    @PreAuthorize("hasPermission(null, 'meeting:view')")
    Response<Meeting> findByMeetingId(@PathVariable String id);

    @ApiOperation(value = "Search meetings")
    @GetMapping("/meets")
    @PreAuthorize("hasPermission(null, 'meeting:view')")
    PagingResponse<Meeting> search(@ValidatePaging(allowedSorts = {"id", "title", "repeatType", "endDate", "createdAt", "createdBy", "lastModifiedAt", "meetingType"})
                                           MeetingSearchRequest searchRequest);
}
