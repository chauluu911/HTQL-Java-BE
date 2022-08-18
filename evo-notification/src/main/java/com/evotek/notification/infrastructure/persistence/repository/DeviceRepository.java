package com.evotek.notification.infrastructure.persistence.repository;

import com.evotek.notification.infrastructure.persistence.entity.DeviceEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface DeviceRepository extends JpaRepository<DeviceEntity, String> {

    @Query("SELECT de from DeviceEntity de where de.userId in (:userIds) and de.deleted = false ")
    List<DeviceEntity> getDeviceEntitiesByUserIds(@Param("userIds") List<String> userIds);

    Optional<DeviceEntity> findByDeviceId(String deviceId);

    @Query("from DeviceEntity de where de.userId = :userId" +
            " and de.deviceId = :deviceId and de.deviceToken = :deviceToken" +
            " and de.deleted = false")
    Optional<DeviceEntity> findByUserIdAndDeviceIdAndDeviceToken(@Param("userId") String userId,
                                                                 @Param("deviceId") String deviceId,
                                                                 @Param("deviceToken") String deviceToken);
}
