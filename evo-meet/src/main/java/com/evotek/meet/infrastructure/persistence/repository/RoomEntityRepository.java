package com.evotek.meet.infrastructure.persistence.repository;

import com.evotek.meet.infrastructure.persistence.entity.RoomEntity;
import com.evotek.meet.infrastructure.persistence.repository.custom.RoomEntityRepositoryCustom;
import com.evotek.meet.infrastructure.support.enums.RoomStatus;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface RoomEntityRepository extends JpaRepository<RoomEntity, String>, RoomEntityRepositoryCustom {

    @Query("from RoomEntity e where e.code = :code and e.deleted = false")
    Optional<RoomEntity> findByCode(String code);

    @Query("from RoomEntity e where e.id in :roomIds and e.deleted = false")
    List<RoomEntity> findByRoomIds(List<String> roomIds);

    @Query("from RoomEntity e where e.deleted = false and " +
            "( :keyword is null or (e.name like :keyword or e.code like :keyword)) and e.status = :status")
    List<RoomEntity> autoComplete(String keyword, RoomStatus status, Pageable pageable);
}
