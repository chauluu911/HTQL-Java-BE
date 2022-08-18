package com.evotek.meet.application.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RoomSchedulerResponse {
    private String id;
    private String meetingId;
    private String roomId;
    private String title;
    private String organizerId;
    private String organizerName;
    private Instant startAt;
    private Instant finishAt;
}
