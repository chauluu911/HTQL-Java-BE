package com.evotek.common.ldapData.infrastructure.presistence.mapper;

import com.evotek.common.ldapData.application.dto.request.GroupLdapCreateOrUpdateRequest;
import com.evotek.common.ldapData.application.dto.request.UserLdapCreateOrUpdateRequest;
import com.evotek.common.ldapData.domain.command.GroupCreateOrUpdateCmd;
import com.evotek.common.ldapData.domain.command.UserCreateOrUpdateCmd;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface AutoMapper {
    GroupCreateOrUpdateCmd from(GroupLdapCreateOrUpdateRequest request);

    UserCreateOrUpdateCmd from(UserLdapCreateOrUpdateRequest request);

}
