package com.evotek.notification.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateUUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotEmpty;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class NotificationDeleteRequest extends Request {

    @NotEmpty(message = "ID_REQUIRED")
    private List<@ValidateUUID String> ids;

}
