package com.evotek.iam.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.iam.domain.EmploymentHistory;
import com.evotek.iam.domain.User;
import com.evotek.iam.domain.UserRole;
import com.evotek.iam.domain.respository.UserDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.EmploymentHistoryEntity;
import com.evotek.iam.infrastructure.persistence.entity.UserEntity;
import com.evotek.iam.infrastructure.persistence.entity.UserRoleEntity;
import com.evotek.iam.infrastructure.persistence.mapper.EmploymentHistoryEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.UserEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.UserRoleEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.EmploymentHistoryRepository;
import com.evotek.iam.infrastructure.persistence.repository.UserEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.UserRoleEntityRepository;
import com.evotek.iam.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Service
@Slf4j
public class UserDomainRepositoryImpl extends AbstractDomainRepository<User, UserEntity, String> implements UserDomainRepository {

    private final UserEntityRepository userEntityRepository;
    private final UserEntityMapper userEntityMapper;
    private final UserRoleEntityMapper userRoleEntityMapper;
    private final UserRoleEntityRepository userRoleEntityRepository;
    private final EmploymentHistoryEntityMapper employmentHistoryEntityMapper;
    private final EmploymentHistoryRepository employmentHistoryRepository;

    public UserDomainRepositoryImpl(UserEntityRepository userEntityRepository,
                                    UserEntityMapper userEntityMapper,
                                    UserRoleEntityMapper userRoleEntityMapper,
                                    UserRoleEntityRepository userRoleEntityRepository,
                                    EmploymentHistoryEntityMapper employmentHistoryEntityMapper,
                                    EmploymentHistoryRepository employmentHistoryRepository) {
        super(userEntityRepository, userEntityMapper);

        this.userEntityRepository = userEntityRepository;
        this.userEntityMapper = userEntityMapper;
        this.userRoleEntityMapper = userRoleEntityMapper;
        this.userRoleEntityRepository = userRoleEntityRepository;
        this.employmentHistoryEntityMapper = employmentHistoryEntityMapper;
        this.employmentHistoryRepository = employmentHistoryRepository;
    }

    @Override
    @Transactional
    public User save(User domain) {
        UserEntity userEntity = userEntityMapper.toEntity(domain);
        this.userEntityRepository.save(userEntity);

        // save user role
        List<UserRole> userRoles = domain.getUserRoles();
        if (!CollectionUtils.isEmpty(userRoles)) {
            List<UserRoleEntity> userRoleEntities = userRoleEntityMapper.toEntity(userRoles);
            this.userRoleEntityRepository.saveAll(userRoleEntities);
        }
        // save department user
        List<EmploymentHistory> employmentHistories = domain.getEmploymentHistories();
        if (!CollectionUtils.isEmpty(employmentHistories)) {
            List<EmploymentHistoryEntity> departmentUserEntities = employmentHistoryEntityMapper.toEntity(employmentHistories);
            this.employmentHistoryRepository.saveAll(departmentUserEntities);
        }

        return domain;
    }

    @Override
    public User getById(String id) {
        return this.findById(id).orElseThrow(() ->
                new ResponseException(NotFoundError.USER_NOT_FOUND));
    }

    @Override
    protected User enrich(User user) {
        // enrich user role
        List<UserRoleEntity> userRoleEntities = userRoleEntityRepository.findAllByUserId(user.getId());
        List<UserRole> userRoles = userRoleEntityMapper.toDomain(userRoleEntities);
        user.enrichUserRoles(userRoles);

        // enrich department user
        List<EmploymentHistoryEntity> departmentUserEntities = employmentHistoryRepository.findAllByUserId(user.getId());
        List<EmploymentHistory> employmentHistories = employmentHistoryEntityMapper.toDomain(departmentUserEntities);
        user.enrichEmploymentHistory(employmentHistories);

        return user;
    }
}

