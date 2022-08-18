package com.evotek.meet.domain.command;

import com.evotek.meet.domain.event.BookedTimeEvent;
import com.evotek.meet.infrastructure.support.enums.MeetingType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserCreateOrUpdateSchedulerCmd {
    private String roomId;
    private String meetingId;
    private MeetingType meetingType;
    private String organizerId;
    private String link;
    private List<BookedTimeEvent> bookedTimeEvents;
}
