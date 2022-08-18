package com.evotek.meet.infrastructure.persistence.repository;

import com.evotek.meet.infrastructure.persistence.entity.MeetingEntity;
import com.evotek.meet.infrastructure.persistence.repository.custom.MeetingEntityRepositoryCustom;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
public interface MeetingEntityRepository extends JpaRepository<MeetingEntity, String>, MeetingEntityRepositoryCustom {
    @Query("from MeetingEntity e where e.id in :ids and e.deleted = false")
    List<MeetingEntity> findByIds(List<String> ids);
}
