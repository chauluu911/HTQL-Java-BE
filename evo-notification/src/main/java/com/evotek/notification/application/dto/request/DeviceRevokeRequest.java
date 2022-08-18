package com.evotek.notification.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.*;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DeviceRevokeRequest extends Request {

    @NotBlank(message = "DEVICE_ID_REQUIRED")
    @Size(min = ValidateConstraint.LENGTH.ID_MIN_LENGTH, max = ValidateConstraint.LENGTH.ID_MAX_LENGTH,
            message = "ID_LENGTH")
    private String deviceId;

    @NotBlank(message = "DEVICE_TOKEN_REQUIRED")
    @Size(min = 1, max = ValidateConstraint.LENGTH.VALUE_MAX_LENGTH, message = "DEVICE_TOKEN_LENGTH")
    private String deviceToken;
}
