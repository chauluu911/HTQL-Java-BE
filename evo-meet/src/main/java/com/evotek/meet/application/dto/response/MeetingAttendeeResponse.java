package com.evotek.meet.application.dto.response;

import com.evotek.meet.infrastructure.support.enums.AttendeeType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MeetingAttendeeResponse {
    private String userId;
    private String fullName;
    private String employeeCode;
    private AttendeeType attendeeType;
}
