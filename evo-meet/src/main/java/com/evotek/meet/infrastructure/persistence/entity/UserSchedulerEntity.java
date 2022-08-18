package com.evotek.meet.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.meet.infrastructure.support.enums.ApproveStatus;
import com.evotek.meet.infrastructure.support.enums.AttendeeType;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.time.Instant;
import java.util.Objects;

@EqualsAndHashCode
@Entity
@Table(name = "user_scheduler", indexes = {
        @Index(name = "user_scheduler_user_id_idx", columnList = "user_id"),
        @Index(name = "user_scheduler_meeting_id_idx", columnList = "meeting_id"),
        @Index(name = "user_scheduler_deleted_idx", columnList = "deleted")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class UserSchedulerEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "user_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String userId;

    @Column(name = "room_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String roomId;

    @Column(name = "meeting_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String meetingId;

    @Column(name = "approve_status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private ApproveStatus approveStatus;

    @Column(name = "link", length = ValidateConstraint.LENGTH.VALUE_MAX_LENGTH)
    private String link;

    @Column(name = "attendee_type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private AttendeeType attendeeType;

    @Column(name = "start_at", nullable = false)
    private Instant startAt;

    @Column(name = "finish_at", nullable = false)
    private Instant finishAt;


    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        UserSchedulerEntity that = (UserSchedulerEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
