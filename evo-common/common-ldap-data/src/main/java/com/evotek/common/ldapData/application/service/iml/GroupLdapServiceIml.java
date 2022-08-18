package com.evotek.common.ldapData.application.service.iml;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.ldapData.application.dto.request.GroupLdapCreateOrUpdateRequest;
import com.evotek.common.ldapData.application.service.GroupLdapService;
import com.evotek.common.ldapData.domain.GroupLdap;
import com.evotek.common.ldapData.domain.UserLdap;
import com.evotek.common.ldapData.domain.command.GroupCreateOrUpdateCmd;
import com.evotek.common.ldapData.infrastructure.presistence.entry.GroupLdapEntry;
import com.evotek.common.ldapData.infrastructure.presistence.entry.UserLdapEntry;
import com.evotek.common.ldapData.infrastructure.presistence.mapper.AutoMapper;
import com.evotek.common.ldapData.infrastructure.presistence.mapper.GroupLdapEntryMapper;
import com.evotek.common.ldapData.infrastructure.presistence.mapper.UserLdapEntryMapper;
import com.evotek.common.ldapData.infrastructure.presistence.repository.GroupLdapRepository;
import com.evotek.common.ldapData.infrastructure.presistence.repository.UserLdapRepository;
import com.evotek.common.ldapData.infrastructure.support.exception.NotFoundError;
import com.evotek.common.ldapData.infrastructure.support.util.Const;
import com.evotek.common.ldapcore.config.LDAPProperties;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.ldap.support.LdapNameBuilder;
import org.springframework.stereotype.Service;

import javax.naming.Name;
import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@EnableConfigurationProperties(LDAPProperties.class)
public class GroupLdapServiceIml implements GroupLdapService {
    private final GroupLdapRepository groupLdapRepository;
    private final UserLdapRepository userLdapRepository;
    private final AutoMapper autoMapper;
    private final GroupLdapEntryMapper groupLdapEntryMapper;
    private final UserLdapEntryMapper userLdapEntryMapper;
    private final LDAPProperties ldapProperties;

    public GroupLdapServiceIml(GroupLdapRepository groupLdapRepository, UserLdapRepository userLdapRepository, AutoMapper autoMapper, GroupLdapEntryMapper groupLdapEntryMapper, UserLdapEntryMapper userLdapEntryMapper, LDAPProperties ldapProperties) {
        this.groupLdapRepository = groupLdapRepository;
        this.userLdapRepository = userLdapRepository;
        this.autoMapper = autoMapper;
        this.groupLdapEntryMapper = groupLdapEntryMapper;
        this.userLdapEntryMapper = userLdapEntryMapper;
        this.ldapProperties = ldapProperties;
    }

    @Override
    public List<GroupLdap> getAllGroupLdap() {
        List<GroupLdap> result = this.groupLdapEntryMapper.toDomain(this.groupLdapRepository.findAll());
        return result;
    }

    @Override
    public GroupLdap create(GroupLdapCreateOrUpdateRequest request) {
        GroupCreateOrUpdateCmd cmd = this.autoMapper.from(request);
        GroupLdap groupLdap = new GroupLdap(cmd, this.ldapProperties);
        GroupLdapEntry groupLdapEntry = this.groupLdapEntryMapper.toEntity(groupLdap);
        groupLdapEntry.setNew(true);
        this.groupLdapRepository.save(groupLdapEntry);
        return groupLdap;
    }

    @Override
    public GroupLdap update(String groupId, GroupLdapCreateOrUpdateRequest request) {
        Optional<GroupLdapEntry> groupLdapEntryOptional = this.groupLdapRepository.findById(buildName(groupId));
        if (groupLdapEntryOptional.isEmpty()) {
            throw new ResponseException(NotFoundError.GROUP_NOT_FOUND);
        }
        GroupLdap groupLdap = this.groupLdapEntryMapper.toDomain(groupLdapEntryOptional.get());
        GroupCreateOrUpdateCmd cmd = this.autoMapper.from(request);
        groupLdap.update(cmd, this.ldapProperties);
        GroupLdapEntry groupLdapEntry = this.groupLdapEntryMapper.toEntity(groupLdap);
        groupLdapEntry.setNew(false);
        this.groupLdapRepository.save(groupLdapEntry);
        return groupLdap;
    }

    @Override
    public void delete(String groupId) {
        Optional<GroupLdapEntry> groupLdapEntryOptional = this.groupLdapRepository.findById(buildName(groupId));
        if (groupLdapEntryOptional.isEmpty()) {
            throw new ResponseException(NotFoundError.GROUP_NOT_FOUND);
        }
        this.groupLdapRepository.delete(groupLdapEntryOptional.get());
    }

    @Override
    public GroupLdap findGroupById(String groupId) {
        Optional<GroupLdapEntry> groupLdapEntryOptional = this.groupLdapRepository.findById(buildName(groupId));
        if (groupLdapEntryOptional.isEmpty()) {
            throw new ResponseException(NotFoundError.GROUP_NOT_FOUND);
        }
        GroupLdap groupLdap = this.groupLdapEntryMapper.toDomain(groupLdapEntryOptional.get());
        return groupLdap;
    }

    @Override
    public void addUserToGroup(String userId, String groupId) {
        Optional<GroupLdapEntry> groupLdapEntryOptional = this.groupLdapRepository.findById(buildName(groupId));
        if (groupLdapEntryOptional.isEmpty()) {
            throw new ResponseException(NotFoundError.GROUP_NOT_FOUND);
        }
        Optional<UserLdapEntry> userLdapEntryOptional = this.userLdapRepository.findById(buildName(userId));
        if (userLdapEntryOptional.isEmpty()) {
            throw new ResponseException(NotFoundError.USER_NOT_FOUND);
        }
        GroupLdap groupLdap = this.groupLdapEntryMapper.toDomain(groupLdapEntryOptional.get());
        UserLdap userLdap = this.userLdapEntryMapper.toDomain(userLdapEntryOptional.get());
        groupLdap.addMember(userLdap);
        GroupLdapEntry groupLdapEntry = this.groupLdapEntryMapper.toEntity(groupLdap);
        groupLdapEntry.setNew(false);
        this.groupLdapRepository.save(groupLdapEntry);
    }

    public Name buildName(String id) {
        return LdapNameBuilder.newInstance()
                .add(Const.COMMON_NAME, id).build();
    }

}
