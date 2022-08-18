package com.evotek.common.ldapData.infrastructure.presistence.repository;


import com.evotek.common.ldapData.infrastructure.presistence.entry.GroupLdapEntry;
import org.springframework.data.ldap.repository.LdapRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface GroupLdapRepository extends LdapRepository<GroupLdapEntry> {
}
