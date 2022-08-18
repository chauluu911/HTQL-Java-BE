package com.evotek.meet.infrastructure.persistence.repository;

import com.evotek.meet.infrastructure.persistence.entity.RoomSchedulerEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.Instant;
import java.util.List;

public interface RoomSchedulerEntityRepository extends JpaRepository<RoomSchedulerEntity, String> {

    @Query("from RoomSchedulerEntity e where e.roomId = :roomId and e.startAt >= :startAt and e.finishAt <= :finishAt and e.deleted = false")
    List<RoomSchedulerEntity> findByStartAtAndFinishAt(String roomId, Instant startAt, Instant finishAt);

    @Query("from RoomSchedulerEntity e where e.roomId = :roomId and e.deleted = false")
    List<RoomSchedulerEntity> findAllByRoomId(String roomId);

    @Query("from RoomSchedulerEntity e where e.roomId in :roomIds and e.deleted = false")
    List<RoomSchedulerEntity> findAllByListRoomId(List<String> roomIds);
}
