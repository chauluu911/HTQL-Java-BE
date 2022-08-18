package com.evotek.notification.presentation.web.rest;

import com.evotek.common.dto.request.LogoutRevokeRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.notification.application.dto.request.DeviceCreateRequest;
import com.evotek.notification.domain.Device;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.validation.Valid;

@Api(tags = "Device Resource")
@RequestMapping("/api")
public interface DeviceResource {

    @ApiOperation("Create Device User")
//    @PreAuthorize("hasPermission(null, 'device:update')")
    @PostMapping("/devices")
    Response<Device> create(@RequestBody @Valid DeviceCreateRequest request);

    @ApiOperation("Revoke Device Token")
    @PreAuthorize("hasPermission(null, 'client:update')")
    @PostMapping("/devices/revoke")
    Response<Boolean> revoke(@RequestBody @Valid LogoutRevokeRequest request);
}
