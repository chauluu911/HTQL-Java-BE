package com.evotek.notification.domain.command;

import com.evotek.common.enums.ContentType;
import com.evotek.common.enums.EventType;
import com.evotek.common.enums.TargetType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class IssueEventCmd {

    private String title;
    private List<EventType> types;
    private String content;
    private ContentType contentType;
    private List<String> targets;
    private TargetType targetType;
    private List<String> attachmentFileIds;
    private List<String> inlineFileIds;

}
