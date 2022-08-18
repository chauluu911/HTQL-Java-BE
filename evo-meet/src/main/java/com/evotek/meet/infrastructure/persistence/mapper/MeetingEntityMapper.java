package com.evotek.meet.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.common.util.StrUtils;
import com.evotek.common.util.StringPool;
import com.evotek.meet.domain.Meeting;
import com.evotek.meet.infrastructure.persistence.entity.MeetingEntity;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.time.DayOfWeek;

@Mapper(componentModel = "spring")
public interface MeetingEntityMapper extends EntityMapper<Meeting, MeetingEntity> {
    @Mapping(source = "dayOfWeeks", target = "dayOfWeeks", qualifiedByName = "dayOfWeeksToString")
    MeetingEntity toEntity(Meeting meeting);

    @Mapping(source = "dayOfWeeks", target = "dayOfWeeks", qualifiedByName = "stringToDaysOfWeeks")
    Meeting toDomain(MeetingEntity meetingEntity);

    @Named("dayOfWeeksToString")
    static String dayOfWeekListToString(List<DayOfWeek> dayOfWeeks) {
        return dayOfWeeks.stream().map(DayOfWeek::toString).collect(Collectors.joining(StringPool.COMMA));
    }

    @Named("stringToDaysOfWeeks")
    static List<DayOfWeek> stringToDaysOfWeekList(String dayOfWeeks) {
        if (!StrUtils.isBlank(dayOfWeeks)) {
            String[] dayOfWeekArray = dayOfWeeks.split(StringPool.COMMA);
            return Arrays.stream(dayOfWeekArray).map(DayOfWeek::valueOf).collect(Collectors.toList());
        }
        return new ArrayList<>();
    }
}
