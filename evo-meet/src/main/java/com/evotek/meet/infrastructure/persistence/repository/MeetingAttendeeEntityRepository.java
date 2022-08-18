package com.evotek.meet.infrastructure.persistence.repository;

import com.evotek.meet.infrastructure.persistence.entity.MeetingAttendeeEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface MeetingAttendeeEntityRepository extends JpaRepository<MeetingAttendeeEntity, String> {

    @Query("from MeetingAttendeeEntity e where e.meetingId = :meetingId and e.deleted = false")
    List<MeetingAttendeeEntity> findByMeetingId(String meetingId);

    @Query("from MeetingAttendeeEntity e where e.userId = :userId and e.meetingId = :meetingId and e.deleted = false")
    List<MeetingAttendeeEntity> findByMeetingIdAndUserId(String meetingId, String userId);

    @Query("from MeetingAttendeeEntity e where e.userId = :userId and e.deleted = false")
    List<MeetingAttendeeEntity> findByUserId(String userId);

    @Query("from MeetingAttendeeEntity e where e.meetingId in :meetingIds and e.deleted = false")
    List<MeetingAttendeeEntity> findByListMeetingIds(List<String> meetingIds);
}
