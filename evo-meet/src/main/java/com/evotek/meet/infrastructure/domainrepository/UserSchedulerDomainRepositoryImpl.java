package com.evotek.meet.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.meet.domain.UserScheduler;
import com.evotek.meet.domain.repository.UserSchedulerDomainRepository;
import com.evotek.meet.infrastructure.persistence.entity.UserSchedulerEntity;
import com.evotek.meet.infrastructure.persistence.mapper.UserSchedulerEntityMapper;
import com.evotek.meet.infrastructure.persistence.repository.UserSchedulerEntityRepository;
import com.evotek.meet.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class UserSchedulerDomainRepositoryImpl extends AbstractDomainRepository<UserScheduler, UserSchedulerEntity, String> implements UserSchedulerDomainRepository {
    private final UserSchedulerEntityMapper userSchedulerEntityMapper;
    private final UserSchedulerEntityRepository userSchedulerEntityRepository;

    public UserSchedulerDomainRepositoryImpl(UserSchedulerEntityMapper userSchedulerEntityMapper, UserSchedulerEntityRepository userSchedulerEntityRepository) {
        super(userSchedulerEntityRepository, userSchedulerEntityMapper);
        this.userSchedulerEntityMapper = userSchedulerEntityMapper;
        this.userSchedulerEntityRepository = userSchedulerEntityRepository;
    }


    @Override
    public UserScheduler getById(String id) {
        return this.findById(id).orElseThrow(() ->
                new ResponseException(NotFoundError.USER_SCHEDULER_NOT_FOUND));
    }
}
