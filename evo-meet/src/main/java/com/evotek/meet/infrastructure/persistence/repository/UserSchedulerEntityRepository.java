package com.evotek.meet.infrastructure.persistence.repository;

import com.evotek.meet.infrastructure.persistence.entity.UserSchedulerEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.Instant;
import java.util.List;

public interface UserSchedulerEntityRepository extends JpaRepository<UserSchedulerEntity, String> {
    @Query("from UserSchedulerEntity e where e.userId in :userIds and e.deleted = false")
    List<UserSchedulerEntity> findAllByUserIdIn(List<String> userIds);

    @Query("from UserSchedulerEntity e where e.userId in :id and e.deleted = false")
    List<UserSchedulerEntity> findByUserId(String id);

    @Query("from UserSchedulerEntity e where e.meetingId = :meetingId and e.deleted = false")
    List<UserSchedulerEntity> findAllByMeetingId(String meetingId);

    @Query("from UserSchedulerEntity e where e.meetingId = :meetingId and e.userId = :currentUserId and e.deleted = false")
    List<UserSchedulerEntity> findByMeetingIdAndCurrentId(String meetingId, String currentUserId);

    @Query("from UserSchedulerEntity e where e.userId = :userId and e.startAt >= :startAt and e.finishAt <= :finishAt and e.approveStatus = 'APPROVED' and e.deleted = false")
    List<UserSchedulerEntity> findByStartAtAndFinishAt(String userId, Instant startAt, Instant finishAt);

}
