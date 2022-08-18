package com.evotek.meet.domain.query;

import com.evotek.common.query.PagingQuery;
import com.evotek.meet.infrastructure.support.enums.RoomStatus;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class RoomSearchQuery extends PagingQuery {
    private String keyword;
    private RoomStatus status;
}
