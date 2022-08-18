package com.evotek.notification.infrastructure.persistence.repository;

import com.evotek.notification.infrastructure.persistence.entity.UserTelegramEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface UserTelegramRepository extends JpaRepository<UserTelegramEntity, String> {

    @Query("from UserTelegramEntity e where e.userId = :userId and e.deleted = false")
    Optional<UserTelegramEntity> findByUserId(String userId);

    @Query("from UserTelegramEntity e where e.userId in :userIds and e.deleted = false")
    List<UserTelegramEntity> findByUserIds(List<String> userIds);

}
