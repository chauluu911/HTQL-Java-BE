package com.evotek.notification.domain.command;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class EventUpdateCommand {

    private String description;

    private String title;

    private String content;

    private Instant expectedNotificationAt;

    private String note;

    private List<String> fileIds;

    private List<String> userIds;

    private List<String> departmentIds;
}
