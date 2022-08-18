package com.evotek.notification.infrastructure.persistence.repository;

import com.evotek.notification.infrastructure.persistence.entity.EventFileEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface EventFileRepository extends JpaRepository<EventFileEntity, String> {
    @Query("update EventFileEntity EF set EF.deleted = true where EF.id in :ids")
    void deleteAllByIds(@Param("ids") List<String> ids);

    @Query("from EventFileEntity e where e.deleted = false and e.eventId = :eventId")
    List<EventFileEntity> findAllByEventId(@Param("eventId") String eventId);

    @Query("from EventFileEntity e where e.deleted = false and e.eventId in :eventIds")
    List<EventFileEntity> findAllByEventIds(@Param("eventIds") List<String> eventIds);

}
