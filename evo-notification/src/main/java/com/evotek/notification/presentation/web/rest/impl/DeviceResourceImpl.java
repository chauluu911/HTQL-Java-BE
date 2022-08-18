package com.evotek.notification.presentation.web.rest.impl;

import com.evotek.common.dto.request.LogoutRevokeRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.notification.application.dto.request.DeviceCreateRequest;
import com.evotek.notification.application.service.DeviceService;
import com.evotek.notification.domain.Device;
import com.evotek.notification.presentation.web.rest.DeviceResource;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
public class DeviceResourceImpl implements DeviceResource {

    private final DeviceService deviceService;

    public DeviceResourceImpl(DeviceService deviceService) {
        this.deviceService = deviceService;
    }

    @Override
    public Response<Device> create(@Valid DeviceCreateRequest request) {
        Device device = deviceService.create(request);
        return Response.of(device);
    }

    @Override
    public Response<Boolean> revoke(@Valid LogoutRevokeRequest request) {
        deviceService.revoke(request);
        return Response.of(Boolean.TRUE);
    }
}
