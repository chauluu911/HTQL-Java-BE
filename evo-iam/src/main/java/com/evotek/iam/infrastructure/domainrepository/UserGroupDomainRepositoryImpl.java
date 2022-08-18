package com.evotek.iam.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.iam.domain.UserGroup;
import com.evotek.iam.domain.UserGroupMember;
import com.evotek.iam.domain.respository.UserGroupDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.UserGroupEntity;
import com.evotek.iam.infrastructure.persistence.entity.UserGroupMemberEntity;
import com.evotek.iam.infrastructure.persistence.mapper.UserGroupEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.UserGroupMemberEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.UserGroupEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.UserGroupMemberEntityRepository;
import com.evotek.iam.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.Arrays;
import java.util.List;

@Service
@Slf4j
public class UserGroupDomainRepositoryImpl extends AbstractDomainRepository<UserGroup, UserGroupEntity, String> implements UserGroupDomainRepository {
    private final UserGroupEntityRepository userGroupEntityRepository;
    private final UserGroupEntityMapper userGroupEntityMapper;
    private final UserGroupMemberEntityRepository userGroupMemberEntityRepository;
    private final UserGroupMemberEntityMapper userGroupMemberEntityMapper;

    protected UserGroupDomainRepositoryImpl(UserGroupEntityRepository userGroupEntityRepository,
                                            UserGroupEntityMapper userGroupEntityMapper,
                                            UserGroupMemberEntityRepository userGroupMemberEntityRepository,
                                            UserGroupMemberEntityMapper userGroupMemberEntityMapper) {
        super(userGroupEntityRepository, userGroupEntityMapper);
        this.userGroupEntityRepository = userGroupEntityRepository;
        this.userGroupEntityMapper = userGroupEntityMapper;
        this.userGroupMemberEntityRepository = userGroupMemberEntityRepository;
        this.userGroupMemberEntityMapper = userGroupMemberEntityMapper;
    }

    @Override
    public UserGroup getById(String id) {
        return this.findById(id).orElseThrow(()->
                new ResponseException((NotFoundError.USER_GROUP_NOT_FOUND)));
    }

    @Override
    public UserGroup save(UserGroup domain) {
        UserGroupEntity userGroupEntity = this.userGroupEntityMapper.toEntity(domain);
        this.userGroupEntityRepository.save(userGroupEntity);
        //save user group member
        List<UserGroupMember> userGroupMemberList = domain.getUserGroupMembers();
        if(!CollectionUtils.isEmpty(userGroupMemberList))
        {
            List<UserGroupMemberEntity> userGroupMemberEntities = this.userGroupMemberEntityMapper.toEntity(userGroupMemberList);
            this.userGroupMemberEntityRepository.saveAll(userGroupMemberEntities);
        }
        return super.save(domain);
    }

    @Override
    protected UserGroup enrich(UserGroup userGroup) {
        // enrich user member
        List<UserGroupMemberEntity> userGroupMemberEntities = this.userGroupMemberEntityRepository.findAllMemberByGroupIds(Arrays.asList(userGroup.getId()));
        List<UserGroupMember> userGroupMembers = this.userGroupMemberEntityMapper.toDomain(userGroupMemberEntities);
        userGroup.enrichUserGroupMembers(userGroupMembers);
        return userGroup;
    }
}
