package com.evotek.meet.domain.command;

import com.evotek.meet.infrastructure.support.enums.MeetingType;
import com.evotek.meet.infrastructure.support.enums.RepeatType;
import com.evotek.meet.infrastructure.support.enums.WeekOfMonth;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.DayOfWeek;
import java.time.Instant;
import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MeetingCreateOrUpdateCmd {

    private String title;
    private String description;
    private Instant startAt;
    private Instant finishAt;
    private LocalDate endDate;
    private RepeatType repeatType;
    private Integer repeatMeasure;
    private List<DayOfWeek> dayOfWeekList;
    private Integer dayOfMonth;
    private WeekOfMonth weekOfMonth;
    private Month monthOfYear;
    private String roomId;
    private String presiderId;
    private MeetingType meetingType;
    private String link;
    private List<String> requiredUserIds = new ArrayList<>();
    private List<String> optionalUserIds = new ArrayList<>();
}
