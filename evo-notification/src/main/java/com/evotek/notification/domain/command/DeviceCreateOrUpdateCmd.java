package com.evotek.notification.domain.command;

import com.evotek.notification.infrastructure.support.enums.DeviceType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DeviceCreateOrUpdateCmd {

    private String name;

    private String deviceId;

    private String deviceToken;

    private DeviceType deviceType;

    private String deviceInfo;

    private String appVersion;
}
