package com.evotek.notification.infrastructure.persistence.repository;

import com.evotek.notification.infrastructure.persistence.entity.EventEntity;
import com.evotek.notification.infrastructure.persistence.repository.custom.EventRepositoryCustom;
import com.evotek.notification.infrastructure.support.enums.EventSource;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface EventRepository extends JpaRepository<EventEntity, String>, EventRepositoryCustom {

    @Query("from EventEntity e where e.id in (:eventIds) and e.deleted = false ")
    List<EventEntity> findAllByIds(@Param("eventIds") List<String> eventIds);

    @Query("from EventEntity e where e.id = :id and e.deleted = false ")
    Optional<EventEntity> findByIdActivated(@Param("id") String id);

    @Query("select distinct(e.id) from EventEntity e where e.deleted = false and e.id in :ids and e.eventSource = :eventSource")
    List<String> findIdByIdsAndEventSource(@Param("ids") List<String> ids, @Param("eventSource") EventSource eventSource);

}
