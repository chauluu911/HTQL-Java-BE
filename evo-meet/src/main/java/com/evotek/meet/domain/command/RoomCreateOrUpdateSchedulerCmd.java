package com.evotek.meet.domain.command;

import com.evotek.meet.infrastructure.support.enums.RepeatType;
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
public class RoomCreateOrUpdateSchedulerCmd {

    private Instant startAt;
    private Instant finishAt;
    private Instant endDate;
    private RepeatType repeatType;
    private Integer repeatMeasure;
    private List<Integer> dayOfWeekList;
    private Integer dayOfMonth;
    private Integer weekOfMonth;
    private Integer monthOfYear;
    private String roomId;
}
