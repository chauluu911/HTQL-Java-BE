package com.evotek.meet.domain.query;

import com.evotek.common.query.PagingQuery;
import com.evotek.meet.infrastructure.support.enums.MeetingStatus;
import com.evotek.meet.infrastructure.support.enums.MeetingType;
import com.evotek.meet.infrastructure.support.enums.RepeatType;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

import java.time.Instant;
import java.time.LocalDate;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class MeetingSearchQuery extends PagingQuery {
    private String keyword;
    private String title;
    private String roomId;
    private RepeatType repeatType;
    private Instant startAt;
    private LocalDate endDate;
    private MeetingType meetingType;
    private MeetingStatus meetingStatus;
    private String userId;
}
