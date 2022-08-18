package com.evotek.notification.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import com.evotek.notification.domain.command.DeviceCreateOrUpdateCmd;
import com.evotek.notification.infrastructure.support.enums.DeviceType;
import lombok.*;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class Device extends AuditableDomain {
    private String id;
    private String name;
    private String userId;
    private String deviceId;
    private String deviceToken;
    private DeviceType deviceType;
    private String deviceInfo;
    private String appVersion;
    private Boolean deleted;

    public Device(String userId, DeviceCreateOrUpdateCmd cmd) {
        this.id = IdUtils.nextId();
        this.name = cmd.getName();
        this.userId = userId;
        this.deviceId = cmd.getDeviceId();
        this.deviceToken = cmd.getDeviceToken();
        this.deviceType = cmd.getDeviceType();
        this.deviceInfo = cmd.getDeviceInfo();
        this.appVersion = cmd.getAppVersion();
        this.deleted = false;
    }

    public void changeUser(String userId, DeviceCreateOrUpdateCmd cmd) {
        this.deviceToken = cmd.getDeviceToken();
        this.deviceType = cmd.getDeviceType();
        this.deviceInfo = cmd.getDeviceInfo();
        this.appVersion = cmd.getAppVersion();
        this.name = cmd.getName();
        this.userId = userId;
        this.deleted = false;
    }

    public void delete() {
        this.deleted = true;
    }
}
