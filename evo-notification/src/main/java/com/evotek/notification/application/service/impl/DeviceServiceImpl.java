package com.evotek.notification.application.service.impl;

import com.evotek.common.dto.request.LogoutRevokeRequest;
import com.evotek.common.error.AuthenticationError;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainService;
import com.evotek.common.webapp.support.SecurityUtils;
import com.evotek.notification.application.dto.request.DeviceCreateRequest;
import com.evotek.notification.application.mapper.AutoMapper;
import com.evotek.notification.application.mapper.DeviceEntityMapper;
import com.evotek.notification.application.service.DeviceService;
import com.evotek.notification.domain.Device;
import com.evotek.notification.domain.command.DeviceCreateOrUpdateCmd;
import com.evotek.notification.infrastructure.persistence.entity.DeviceEntity;
import com.evotek.notification.infrastructure.persistence.repository.DeviceRepository;
import com.evotek.notification.infrastructure.support.exception.BadRequestError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Service
@Slf4j
public class DeviceServiceImpl extends AbstractDomainService<Device, DeviceEntity, String> implements DeviceService {

    private final DeviceRepository deviceRepository;

    private final DeviceEntityMapper deviceEntityMapper;

    private final AutoMapper autoMapper;

    public DeviceServiceImpl(DeviceRepository deviceRepository,
                             DeviceEntityMapper deviceEntityMapper,
                             AutoMapper autoMapper) {
        super(deviceRepository, deviceEntityMapper);
        this.deviceRepository = deviceRepository;
        this.deviceEntityMapper = deviceEntityMapper;
        this.autoMapper = autoMapper;
    }

    @Override
    @Transactional
    public Device create(DeviceCreateRequest request) {
        // find id of current user login to register device user
        String currentUserLoginId = SecurityUtils.getCurrentUserLoginId()
                .orElseThrow(() -> new ResponseException(AuthenticationError.UNAUTHORISED));
        log.info("User {} register device {}", currentUserLoginId, request.getDeviceId());

        Device device;
        Optional<DeviceEntity> userDeviceEntityOptional = deviceRepository.findByDeviceId(request.getDeviceId());
        DeviceCreateOrUpdateCmd cmd = this.autoMapper.from(request);
        if (userDeviceEntityOptional.isPresent()) {
            device = deviceEntityMapper.toDomain(userDeviceEntityOptional.get());
            device.changeUser(currentUserLoginId, cmd);
            log.info("User {} register device {}, update new owner", currentUserLoginId, request.getDeviceId());
        } else {
            device = new Device(currentUserLoginId, cmd);
            log.info("User {} register device {}, new device", currentUserLoginId, request.getDeviceId());
        }

        save(device);
        return device;
    }

    @Override
    @Transactional
    public Device revoke(LogoutRevokeRequest request) {
        log.info("User {} revoke device {}", request.getUserId(), request.getDeviceId());

        Device device;
        Optional<DeviceEntity> userDeviceEntityOptional = deviceRepository
                .findByUserIdAndDeviceIdAndDeviceToken(request.getUserId(), request.getDeviceId(), request.getDeviceToken());
        if (userDeviceEntityOptional.isEmpty()) {
            log.info("User {} revoke device {}, device not found", request.getUserId(), request.getDeviceId());
            throw new ResponseException(BadRequestError.DEVICE_TOKEN_NOT_FOUND);
        } else {
            device = deviceEntityMapper.toDomain(userDeviceEntityOptional.get());
        }

        // delete device
        device.delete();
        save(device);
        log.info("User {} revoke device {}, success", request.getUserId(), request.getDeviceId());
        return device;
    }
}
