package com.evotek.meet.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.meet.infrastructure.support.enums.ApproveStatus;
import com.evotek.meet.infrastructure.support.enums.MeetingType;
import com.evotek.meet.infrastructure.support.enums.RepeatType;
import com.evotek.meet.infrastructure.support.enums.WeekOfMonth;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.time.DayOfWeek;
import java.time.Instant;
import java.time.LocalDate;
import java.time.Month;
import java.util.ArrayList;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class MeetingCreateOrUpdateRequest extends Request {

    @NotNull(message = "REQUIRE_USERS_REQUIRED")
    List<String> requiredUserIds = new ArrayList<>();

    List<String> optionalUserIds = new ArrayList<>();

    @NotBlank(message = "MEETING_TITLE_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.TITLE_MAX_LENGTH, message = "TITLE_LENGTH")
    private String title;

    @NotNull(message = "REPEAT_TYPE_REQUIRED")
    private RepeatType repeatType;

    @NotNull(message = "STARTED_TIME_REQUIRED")
    private Instant startAt;

    @NotNull(message = "FINISHED_TIME_REQUIRED")
    private Instant finishAt;

    private String presiderId;

    private String roomId;

    private MeetingType meetingType;

    private ApproveStatus approveStatus;

    private String link;

    private LocalDate endDate;

    private String description;

    private Integer repeatMeasure;

    private List<DayOfWeek> dayOfWeekList = new ArrayList<>();

    private Integer dayOfMonth;

    private WeekOfMonth weekOfMonth;

    private Month monthOfYear;
}
