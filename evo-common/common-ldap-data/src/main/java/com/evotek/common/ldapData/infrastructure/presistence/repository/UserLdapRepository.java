package com.evotek.common.ldapData.infrastructure.presistence.repository;


import com.evotek.common.ldapData.infrastructure.presistence.entry.UserLdapEntry;
import org.springframework.data.ldap.repository.LdapRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface UserLdapRepository extends LdapRepository<UserLdapEntry> {
}
