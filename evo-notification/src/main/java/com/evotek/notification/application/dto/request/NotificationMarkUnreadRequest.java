package com.evotek.notification.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateUUID;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class NotificationMarkUnreadRequest extends Request {

    @Valid
    @NotEmpty(message = "ID_REQUIRED")
    private List<@ValidateUUID String> ids;
}
