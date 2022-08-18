package com.evotek.notification.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.notification.infrastructure.support.enums.DeviceType;
import lombok.*;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DeviceCreateRequest extends Request {

    @NotBlank(message = "NAME_REQUIRED")
    @Size(min = ValidateConstraint.LENGTH.ID_MIN_LENGTH, max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "NAME_MAX_LENGTH")
    private String name;

    @NotBlank(message = "DEVICE_ID_REQUIRED")
    @Size(min = ValidateConstraint.LENGTH.ID_MIN_LENGTH, max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH,
            message = "ID_LENGTH")
    private String deviceId;

    @NotBlank(message = "DEVICE_TOKEN_REQUIRED")
    @Size(min = 1, max = ValidateConstraint.LENGTH.VALUE_MAX_LENGTH, message = "DEVICE_TOKEN_LENGTH")
    private String deviceToken;

    @NotNull
    private DeviceType deviceType;

    @NotBlank(message = "DEVICE_INFO_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.CONTENT_MAX_LENGTH, message = "DEVICE_INFO_LENGTH")
    private String deviceInfo;

    @NotBlank(message = "APP_VERSION_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "APP_VERSION_LENGTH")
    private String appVersion;
}
