package com.evotek.meet.application.dto.response;

import com.evotek.meet.domain.Room;
import com.evotek.meet.infrastructure.support.enums.ApproveStatus;
import com.evotek.meet.infrastructure.support.enums.AttendeeType;
import com.evotek.meet.infrastructure.support.enums.MeetingType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserSchedulerResponse {
    private String id;
    private String userId;
    private String meetingId;
    private String roomId;
    private Room room;
    private String title;
    private String organizerName;
    private List<MeetingAttendeeResponse> meetingAttendees;
    private Instant startAt;
    private Instant finishAt;
    private AttendeeType attendeeType;
    private ApproveStatus approveStatus;
    private MeetingType meetingType;
    private String link;
}
