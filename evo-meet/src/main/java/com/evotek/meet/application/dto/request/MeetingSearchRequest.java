package com.evotek.meet.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import com.evotek.meet.infrastructure.support.enums.MeetingStatus;
import com.evotek.meet.infrastructure.support.enums.MeetingType;
import com.evotek.meet.infrastructure.support.enums.RepeatType;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.time.Instant;
import java.time.LocalDate;

@EqualsAndHashCode(callSuper = true)
@Data
public class MeetingSearchRequest extends PagingRequest {

    private String keyword;
    private String title;
    private String roomId;
    private RepeatType repeatType;
    private Instant startAt;
    private LocalDate endDate;
    private MeetingType meetingType;
    private MeetingStatus meetingStatus;
    private Boolean isSearchOfMe;
}
