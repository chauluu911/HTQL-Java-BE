package com.evotek.meet.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.meet.infrastructure.support.enums.MeetingStatus;
import com.evotek.meet.infrastructure.support.enums.MeetingType;
import com.evotek.meet.infrastructure.support.enums.RepeatType;
import com.evotek.meet.infrastructure.support.enums.WeekOfMonth;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.time.Instant;
import java.time.LocalDate;
import java.time.Month;
import java.util.Objects;

@Entity
@Table(name = "meeting", indexes = {
        @Index(name = "meeting_room_id_idx", columnList = "room_id"),
        @Index(name = "meeting_deleted_idx", columnList = "deleted"),
        @Index(name = "meeting_organizer_id_idx", columnList = "organizer_id")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class MeetingEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "room_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String roomId;

    @Column(name = "organizer_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String organizerId;

    @Column(name = "presider_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String presiderId;

    @Column(name = "title", length = ValidateConstraint.LENGTH.TITLE_MAX_LENGTH, nullable = false)
    private String title;

    @Column(name = "description", length = ValidateConstraint.LENGTH.DESC_MAX_LENGTH)
    private String description;

    @Column(name = "start_at", nullable = false)
    private Instant startAt;

    @Column(name = "finish_at", nullable = false)
    private Instant finishAt;

    @Column(name = "end_date", nullable = false)
    private LocalDate endDate;

    @Column(name = "repeat_type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private RepeatType repeatType;

    @Column(name = "meeting_type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private MeetingType meetingType;

    @Column(name = "meeting_status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private MeetingStatus meetingStatus;

    @Column(name = "link", length = ValidateConstraint.LENGTH.VALUE_MAX_LENGTH)
    private String link;

    @Column(name = "repeat_measure")
    private Integer repeatMeasure;

    @Column(name = "day_of_weeks", length = ValidateConstraint.LENGTH.VALUE_MAX_LENGTH)
    private String dayOfWeeks;

    @Column(name = "day_of_month")
    private Integer dayOfMonth;

    @Column(name = "week_of_month", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH)
    @Enumerated(EnumType.STRING)
    private WeekOfMonth weekOfMonth;

    @Column(name = "month_of_year", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH)
    @Enumerated(EnumType.STRING)
    private Month monthOfYear;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        MeetingEntity that = (MeetingEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }

}
