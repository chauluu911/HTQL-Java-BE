package com.evotek.notification.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.enums.EventType;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.common.validator.ValidateUUID;
import com.evotek.notification.infrastructure.support.enums.EventSendTo;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.time.Instant;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class EventCreateRequest extends Request {

    @NotBlank(message = "TITLE_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.TITLE_MAX_LENGTH, message = "TITLE_MAX_LENGTH")
    private String title;

    @NotNull(message = "EVENT_TYPE_REQUIRED")
    private List<EventType> types;

    @NotNull(message = "EVENT_SEND_TO_REQUIRED")
    private EventSendTo sendTo;

    @NotNull(message = "EXPECT_NOTIFICATION_AT_REQUIRED")
    private Instant expectedNotificationAt;

    @NotBlank(message = "CONTENT_REQUIRED")
    private String content;

    @Size(max = ValidateConstraint.LENGTH.NOTE_MAX_LENGTH, message = "NOTE_MAX_LENGTH")
    private String note;

    @Valid
    @Size(max = 5, message = "FILE_MAX_LENGTH")
    private List<@ValidateUUID String> fileIds;

    @Valid
    private List<@ValidateUUID String> userIds;

    @Valid
    private List<@ValidateUUID String> departmentIds;

}
