package com.evotek.meet.infrastructure.persistence.repository.custom;

import com.evotek.meet.infrastructure.persistence.entity.MeetingEntity;
import com.evotek.meet.domain.query.MeetingSearchQuery;

import java.util.List;

public interface MeetingEntityRepositoryCustom {

    List<MeetingEntity> search(MeetingSearchQuery query);

    Long count(MeetingSearchQuery query);
}
