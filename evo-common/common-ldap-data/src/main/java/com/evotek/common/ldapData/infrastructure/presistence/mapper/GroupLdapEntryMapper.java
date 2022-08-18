package com.evotek.common.ldapData.infrastructure.presistence.mapper;

import com.evotek.common.ldapData.domain.GroupLdap;
import com.evotek.common.ldapData.infrastructure.presistence.entry.GroupLdapEntry;
import com.evotek.common.mapper.EntityMapper;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface GroupLdapEntryMapper extends EntityMapper<GroupLdap, GroupLdapEntry> {
}
