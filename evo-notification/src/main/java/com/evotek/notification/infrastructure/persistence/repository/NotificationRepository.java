package com.evotek.notification.infrastructure.persistence.repository;

import com.evotek.notification.infrastructure.persistence.entity.NotificationEntity;
import com.evotek.notification.infrastructure.persistence.repository.custom.NotificationRepositoryCustom;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface NotificationRepository extends JpaRepository<NotificationEntity, String>, NotificationRepositoryCustom {

    @Query("from NotificationEntity e where e.deleted = false and e.isSend = true and e.userId = :userId")
    List<NotificationEntity> findAllByUserId(@Param("userId") String userId);

    @Query("from NotificationEntity e where e.deleted = false and e.isSend = true and e.userId = :userId and e.eventId in :eventIds")
    List<NotificationEntity> findAllByUserIdAndEventIds(@Param("userId") String userId, @Param("eventIds") List<String> eventIds);

    @Query("from NotificationEntity e where e.deleted = false and e.isSend = true and e.userId = :userId and e.eventId = :eventId")
    Optional<NotificationEntity> findByUserIdAndEventId(@Param("userId") String userId, @Param("eventId") String eventId);

    @Query("from NotificationEntity e where e.deleted = false and e.isSend = true and e.id in :ids and e.userId = :userId")
    List<NotificationEntity> findAllByIdsAndUserId(@Param("ids") List<String> ids, @Param("userId") String userId);

    @Query("from NotificationEntity e where e.deleted = false and e.isSend = true and e.id = :id and e.userId = :userId")
    Optional<NotificationEntity> findByIdAndUserId(@Param("id") String id, @Param("userId") String userId);

    @Query("from NotificationEntity n where n.eventId = :id")
    List<NotificationEntity> findAllByEventId(@Param("id") String id);

    @Query(" SELECT COUNT (e) from NotificationEntity e where e.deleted = false and e.isSend = true and e.isRead = false " +
            "and e.userId = :userId")
    Long countUnreadNotification(@Param("userId") String userId);

    @Query("from NotificationEntity e where e.deleted = false and e.isSend = true and e.isRead = false and e.userId = :userId")
    List<NotificationEntity> findAllUnreadNotiByUserId(@Param("userId") String userId);

}
