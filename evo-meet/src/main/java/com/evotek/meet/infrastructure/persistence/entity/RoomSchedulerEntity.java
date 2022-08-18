package com.evotek.meet.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.time.Instant;
import java.util.Objects;

@Entity
@Table(name = "room_scheduler", indexes = {
        @Index(name = "room_scheduler_meeting_id_idx", columnList = "meeting_id"),
        @Index(name = "room_scheduler_room_id_idx", columnList = "room_id"),
        @Index(name = "room_scheduler_deleted_idx", columnList = "deleted")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class RoomSchedulerEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "meeting_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String meetingId;

    @Column(name = "room_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String roomId;

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
        RoomSchedulerEntity that = (RoomSchedulerEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
