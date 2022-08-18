package com.evotek.notification.presentation.web.rest;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.notification.application.dto.request.NotificationDeleteRequest;
import com.evotek.notification.application.dto.request.NotificationMarkReadRequest;
import com.evotek.notification.application.dto.request.NotificationMarkUnreadRequest;
import com.evotek.notification.application.dto.request.NotificationSearchRequest;
import com.evotek.notification.domain.Notification;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@Api(tags = "Notification Resource")
@RequestMapping("/api")
@Validated
public interface NotificationResource {

    @ApiOperation(value = "Find notification by user login with paging")
    @GetMapping("/me/notifications")
    PagingResponse<Notification> findAllByUserLogin(@ValidatePaging NotificationSearchRequest params);

    @ApiOperation(value = "Find notification by event id")
    @GetMapping("/me/notifications/read-by-event-id/{eventId}")
    Response<Notification> findByEventId(@PathVariable String eventId);

    @ApiOperation(value = "Find notification by id")
    @GetMapping("/me/notifications/{id}")
    Response<Notification> findById(@PathVariable String id);

    @ApiOperation(value = "Mark as read notification by ids")
    @PostMapping("/me/notifications/mark-read-by-ids")
    Response<Boolean> markReadByIds(@RequestBody @Valid NotificationMarkReadRequest notificationMarkReadRequest);

    @ApiOperation(value = "Mark as unread notification by Ids")
    @PostMapping("/me/notifications/mark-unread-by-ids")
    Response<Boolean> markUnReadByIds(@RequestBody @Valid NotificationMarkUnreadRequest request);

    @ApiOperation(value = "Mark as read notification")
    @PostMapping("/me/notifications/{id}/mark-read")
    Response<Boolean> markRead(@PathVariable String id);

    @ApiOperation(value = "Mark as read all notification")
    @PostMapping("/me/notifications/mark-read-all")
    Response<Boolean> markReadAll();

    @ApiOperation(value = "Delete notification")
    @PostMapping("/me/notifications/{id}")
    Response<Boolean> delete(@PathVariable String id);

    @ApiOperation(value = "Delete all notification")
    @PostMapping("/me/notifications/delete-by-ids")
    Response<Boolean> deleteAll(@RequestBody @Valid NotificationDeleteRequest notificationDeleteRequest);


    @ApiOperation(value = "Count unread notification")
    @GetMapping("/me/notifications/count-unread")
    Response<Long> countUnreadNotification();

    @ApiOperation(value = "Get the url to join telegram bot")
    @GetMapping("/me/notifications/url-join-telegram-bot")
    Response<String> getUrlJoinTelegramBot();
}
