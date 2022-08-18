package com.evotek.meet.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.meet.domain.User;
import com.evotek.meet.domain.repository.UserDomainRepository;
import com.evotek.meet.infrastructure.persistence.entity.UserEntity;
import com.evotek.meet.infrastructure.persistence.mapper.UserEntityMapper;
import com.evotek.meet.infrastructure.persistence.repository.UserEntityRepository;
import com.evotek.meet.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class UserDomainRepositoryImpl extends AbstractDomainRepository<User, UserEntity, String> implements UserDomainRepository {

    private final UserEntityMapper userEntityMapper;
    private final UserEntityRepository userEntityRepository;

    public UserDomainRepositoryImpl(UserEntityMapper userEntityMapper, UserEntityRepository userEntityRepository) {
        super(userEntityRepository, userEntityMapper);
        this.userEntityMapper = userEntityMapper;
        this.userEntityRepository = userEntityRepository;
    }


    @Override
    public User getById(String id) {
        return this.findById(id).orElseThrow(() ->
                new ResponseException(NotFoundError.USER_NOT_FOUND));
    }
}
