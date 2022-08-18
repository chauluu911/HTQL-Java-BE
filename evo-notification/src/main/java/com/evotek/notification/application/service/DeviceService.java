package com.evotek.notification.application.service;

import com.evotek.common.dto.request.LogoutRevokeRequest;
import com.evotek.common.webapp.support.DomainService;
import com.evotek.notification.application.dto.request.DeviceCreateRequest;
import com.evotek.notification.domain.Device;

public interface DeviceService extends DomainService<Device, String> {
    /**
     * register device for user
     *
     * @param request
     * @return
     */
    Device create(DeviceCreateRequest request);

    /**
     * revoke token khoi device
     *
     * @param request
     * @return
     */
    Device revoke(LogoutRevokeRequest request);
}
