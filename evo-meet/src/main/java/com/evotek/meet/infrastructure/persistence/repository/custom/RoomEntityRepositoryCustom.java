package com.evotek.meet.infrastructure.persistence.repository.custom;

import com.evotek.meet.application.dto.request.RoomSearchRequest;
import com.evotek.meet.infrastructure.persistence.entity.RoomEntity;

import java.util.List;

public interface RoomEntityRepositoryCustom {

    List<RoomEntity> search(RoomSearchRequest searchRequest);

    Long count(RoomSearchRequest searchRequest);
}
