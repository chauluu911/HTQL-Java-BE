package com.evotek.meet.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.meet.domain.Room;
import com.evotek.meet.domain.repository.RoomDomainRepository;
import com.evotek.meet.infrastructure.persistence.entity.RoomEntity;
import com.evotek.meet.infrastructure.persistence.mapper.RoomEntityMapper;
import com.evotek.meet.infrastructure.persistence.repository.RoomEntityRepository;
import com.evotek.meet.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class RoomDomainRepositoryImpl extends AbstractDomainRepository<Room, RoomEntity, String> implements RoomDomainRepository {

    private final RoomEntityMapper roomEntityMapper;
    private final RoomEntityRepository roomEntityRepository;

    public RoomDomainRepositoryImpl(RoomEntityMapper roomEntityMapper, RoomEntityRepository roomEntityRepository) {
        super(roomEntityRepository, roomEntityMapper);
        this.roomEntityMapper = roomEntityMapper;
        this.roomEntityRepository = roomEntityRepository;
    }

    @Override
    public Room getById(String id) {
        return this.findById(id).orElseThrow(() ->
                new ResponseException(NotFoundError.ROOM_NOT_FOUND));
    }
}
