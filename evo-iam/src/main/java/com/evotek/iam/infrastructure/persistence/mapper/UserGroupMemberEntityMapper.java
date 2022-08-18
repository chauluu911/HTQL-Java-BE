package com.evotek.iam.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.iam.domain.UserGroupMember;
import com.evotek.iam.infrastructure.persistence.entity.UserGroupMemberEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface UserGroupMemberEntityMapper extends EntityMapper<UserGroupMember, UserGroupMemberEntity> {
}