package com.evotek.meet.infrastructure.persistence.repository;

import com.evotek.meet.infrastructure.persistence.entity.UserEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface UserEntityRepository extends JpaRepository<UserEntity, String> {
    @Query("from UserEntity e where e.id in :ids and e.deleted = false")
    List<UserEntity> findAllByUserIdIn(List<String> ids);
}
