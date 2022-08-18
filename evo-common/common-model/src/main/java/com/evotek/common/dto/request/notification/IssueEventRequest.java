package com.evotek.common.dto.request.notification;

import com.evotek.common.dto.request.Request;
import com.evotek.common.enums.ContentType;
import com.evotek.common.enums.EventType;
import com.evotek.common.enums.TargetType;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.List;
import java.util.Set;

@EqualsAndHashCode(callSuper = true)
@Builder
@Data
public class IssueEventRequest extends Request {

    @NotBlank(message = "TITLE_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.TITLE_MAX_LENGTH, message = "TITLE_MAX_LENGTH")
    private String title;

    @NotNull(message = "EVENT_TYPE_REQUIRED")
    private Set<EventType> types;

    @NotBlank(message = "CONTENT_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.CONTENT_MAX_LENGTH, message = "CONTENT_MAX_LENGTH")
    private String content;

    private ContentType contentType;

    private List<String> targets;

    private TargetType targetType;

    private List<String> attachmentFileIds;

    private List<String> inlineFileIds;

}
