package com.evotek.iam.application.service.impl;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.exception.ResponseException;
import com.evotek.iam.application.dto.request.GroupSearchRequest;
import com.evotek.iam.application.dto.request.UserGroupCreateRequest;
import com.evotek.iam.application.dto.request.UserGroupMemberUpdateRequest;
import com.evotek.iam.application.dto.request.UserGroupUpdateRequest;
import com.evotek.iam.application.mapper.AutoMapper;
import com.evotek.iam.application.mapper.AutoMapperQuery;
import com.evotek.iam.application.service.UserGroupService;
import com.evotek.iam.domain.User;
import com.evotek.iam.domain.UserGroup;
import com.evotek.iam.domain.UserGroupMember;
import com.evotek.iam.domain.command.UserGroupCreateCmd;
import com.evotek.iam.domain.query.UserGroupSearchQuery;
import com.evotek.iam.domain.respository.UserGroupDomainRepository;
import com.evotek.iam.infrastructure.persistence.mapper.UserEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.UserGroupEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.UserEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.UserGroupEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.UserGroupMemberEntityRepository;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import com.evotek.iam.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.transaction.Transactional;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@Slf4j
public class UserGroupServiceImpl implements UserGroupService {
    private final UserGroupEntityRepository userGroupEntityRepository;
    private final UserGroupDomainRepository userGroupDomainRepository;
    private final UserGroupEntityMapper userGroupEntityMapper;
    private final UserEntityRepository userEntityRepository;
    private final UserGroupMemberEntityRepository userGroupMemberEntityRepository;
    private final UserEntityMapper userEntityMapper;
    private final AutoMapper autoMapper;
    private final AutoMapperQuery autoMapperQuery;

    public UserGroupServiceImpl(UserGroupEntityRepository userGroupEntityRepository,
                                UserGroupDomainRepository userGroupDomainRepository,
                                UserGroupEntityMapper userGroupEntityMapper,
                                UserEntityRepository userEntityRepository,
                                UserGroupMemberEntityRepository userGroupMemberEntityRepository,
                                UserEntityMapper userEntityMapper,
                                AutoMapper autoMapper,
                                AutoMapperQuery autoMapperQuery) {
        this.userGroupEntityRepository = userGroupEntityRepository;
        this.userGroupDomainRepository = userGroupDomainRepository;
        this.userGroupEntityMapper = userGroupEntityMapper;
        this.userEntityRepository = userEntityRepository;
        this.userGroupMemberEntityRepository = userGroupMemberEntityRepository;
        this.userEntityMapper = userEntityMapper;
        this.autoMapper = autoMapper;
        this.autoMapperQuery = autoMapperQuery;
    }

    @Override
    public PagingResponse<UserGroup> search(GroupSearchRequest request) {
        UserGroupSearchQuery query = this.autoMapperQuery.toQuery(request);
        List<UserGroup> userGroups = this.userGroupEntityMapper.toDomain(this.userGroupEntityRepository.search(query));
        return new PagingResponse<>(userGroups, request.getPageIndex(), request.getPageSize(), this.userGroupEntityRepository.countGroup(query));
    }

    @Transactional
    @Override
    public UserGroup create(UserGroupCreateRequest request) {
        UserGroupCreateCmd cmd = this.autoMapper.from(request);
        if (!CollectionUtils.isEmpty(request.getUserMemberIds())) {
            List<User> userList = this.userEntityMapper.toDomain(this.userEntityRepository.findByIds(request.getUserMemberIds()));
            if (userList.size() != request.getUserMemberIds().size()) {
                throw new ResponseException(BadRequestError.USER_MEMBER_INVALID);
            }
        }
        List<User> users = this.userEntityMapper.toDomain(this.userEntityRepository.findAllUserActive());
        UserGroup userGroup = new UserGroup(cmd, users);
        this.userGroupDomainRepository.save(userGroup);
        return userGroup;
    }

    @Transactional
    @Override
    public UserGroup update(String groupId, UserGroupUpdateRequest request) {
        UserGroup userGroup = this.getUserGroupById(groupId);
        List<User> users = this.userEntityMapper.toDomain(this.userEntityRepository.findAllUserActive());
        userGroup.update(this.autoMapper.from(request), users);
        log.info(userGroup.toString());
        this.userGroupDomainRepository.save(userGroup);
        return userGroup;
    }

    public UserGroup getUserGroupById(String groupId) {
        Optional<UserGroup> userGroupOptional = this.userGroupDomainRepository.findById(groupId);
        if (userGroupOptional.isEmpty()) {
            throw new ResponseException(NotFoundError.USER_GROUP_NOT_FOUND);
        }
        return userGroupOptional.get();
    }

    @Transactional
    @Override
    public void delete(String groupId) {
        Optional<UserGroup> groupUserOptional = this.userGroupDomainRepository.findById(groupId);
        if (groupUserOptional.isEmpty()) {
            throw new ResponseException(NotFoundError.USER_GROUP_NOT_FOUND);
        }
        UserGroup userGroup = groupUserOptional.get();
        userGroup.deleted();
        this.userGroupDomainRepository.save(userGroup);
    }

    @Override
    public UserGroup findById(String groupId) {
        Optional<UserGroup> groupUserEntityOptional = this.userGroupDomainRepository.findById(groupId);
        if (groupUserEntityOptional.isEmpty()) {
            throw new ResponseException(NotFoundError.USER_GROUP_NOT_FOUND);
        }
        UserGroup userGroup = groupUserEntityOptional.get();
        //enrich member
        List<String> userMemberIds = userGroup.getUserGroupMembers().stream().map(UserGroupMember::getUserId).distinct().collect(Collectors.toList());
        List<User> users = this.userEntityMapper.toDomain(this.userEntityRepository.findByIds(userMemberIds));
        userGroup.enrichMembers(users);
        return userGroup;
    }

    @Transactional
    @Override
    public void addListUserToGroup(String groupId, UserGroupMemberUpdateRequest request) {
        UserGroup userGroup = this.getUserGroupById(groupId);
        List<User> users = this.userEntityMapper.toDomain(this.userEntityRepository.findAllUserActive());
        userGroup.addListMemberToGroup(this.autoMapper.from(request), users);
        this.userGroupDomainRepository.save(userGroup);
    }

    @Transactional
    @Override
    public void removeListUserToGroup(String groupId, UserGroupMemberUpdateRequest request) {
        UserGroup userGroup = this.getUserGroupById(groupId);
        List<User> users = this.userEntityMapper.toDomain(this.userEntityRepository.findAllUserActive());
        userGroup.removeListMemberFromGroup(this.autoMapper.from(request), users);
        this.userGroupDomainRepository.save(userGroup);

    }
}
