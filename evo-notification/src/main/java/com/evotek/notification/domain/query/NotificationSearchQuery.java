package com.evotek.notification.domain.query;

import com.evotek.common.query.PagingQuery;
import lombok.Data;
import lombok.experimental.SuperBuilder;

import java.time.Instant;

@Data
@SuperBuilder
public class NotificationSearchQuery extends PagingQuery {
    private String userId;
    private Instant sendStartAt;
    private Instant sendEndAt;
    private Boolean isRead;
}
