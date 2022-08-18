package com.evotek.common.ldapData.infrastructure.presistence.mapper;

import com.evotek.common.ldapData.domain.UserLdap;
import com.evotek.common.ldapData.infrastructure.presistence.entry.UserLdapEntry;
import com.evotek.common.mapper.EntityMapper;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface UserLdapEntryMapper extends EntityMapper<UserLdap, UserLdapEntry> {

}
