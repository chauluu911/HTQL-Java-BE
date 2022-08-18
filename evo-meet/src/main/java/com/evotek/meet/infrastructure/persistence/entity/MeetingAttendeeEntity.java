package com.evotek.meet.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.meet.infrastructure.support.enums.ApproveStatus;
import com.evotek.meet.infrastructure.support.enums.AttendeeType;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "meeting_attendee", indexes = {
        @Index(name = "meeting_attendee_user_id_idx", columnList = "user_id"),
        @Index(name = "meeting_attendee_meeting_id_idx", columnList = "meeting_id"),
        @Index(name = "meeting_attendee_deleted_idx", columnList = "deleted")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class MeetingAttendeeEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "user_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String userId;

    @Column(name = "meeting_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String meetingId;

    @Column(name = "attendee_type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private AttendeeType attendeeType;

    @Column(name = "approve_status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private ApproveStatus approveStatus;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        MeetingAttendeeEntity that = (MeetingAttendeeEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
