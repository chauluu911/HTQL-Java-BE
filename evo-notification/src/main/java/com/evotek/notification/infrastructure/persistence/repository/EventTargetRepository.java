package com.evotek.notification.infrastructure.persistence.repository;

import com.evotek.common.enums.TargetType;
import com.evotek.notification.infrastructure.persistence.entity.EventTargetEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface EventTargetRepository extends JpaRepository<EventTargetEntity, String> {

    @Query("from EventTargetEntity e where e.deleted = false and e.targetType = :targetType and e.eventId = :eventId")
    List<EventTargetEntity> findByTargetTypeAndEventId(@Param("targetType") TargetType targetType, @Param("eventId") String eventId);

    @Query("from EventTargetEntity e where e.deleted = false and e.eventId in :eventIds")
    List<EventTargetEntity> findByEventIds(@Param("eventIds") List<String> eventIds);

    @Query("from EventTargetEntity e where e.deleted = false and e.eventId = :eventId")
    List<EventTargetEntity> findByEventId(@Param("eventId") String eventId);

    @Query("select distinct(e.eventId) from EventTargetEntity e where e.deleted = false and e.target in :targetIds and e.targetType = :targetType")
    List<String> findEventIdsByTargetIdsAndTargetType(@Param("targetIds") List<String> targetIds, @Param("targetType") TargetType targetType);

}
