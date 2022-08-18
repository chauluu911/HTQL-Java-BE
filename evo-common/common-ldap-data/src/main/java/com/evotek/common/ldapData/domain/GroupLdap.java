package com.evotek.common.ldapData.domain;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.ldapData.domain.command.GroupCreateOrUpdateCmd;
import com.evotek.common.ldapData.infrastructure.support.exception.BadRequestError;
import com.evotek.common.ldapData.infrastructure.support.util.Const;
import com.evotek.common.ldapcore.config.LDAPProperties;
import com.evotek.common.util.StringPool;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.ldap.support.LdapNameBuilder;

import javax.naming.Name;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Getter
@Setter
@ToString
@NoArgsConstructor
public class GroupLdap {
    // build by name
    @JsonIgnore
    private Name id;
    private String name;
    @JsonIgnore
    private List<String> members;
    private String description;
    @JsonIgnore
    private LDAPProperties ldapProperties;

    public GroupLdap(GroupCreateOrUpdateCmd cmd, LDAPProperties ldapProperties) {
        this.id = this.buildDn(cmd.getName());
        this.name = cmd.getName();
        this.ldapProperties = ldapProperties;
        this.members = this.buildListMember(this.getMembers());
        this.description = cmd.getDescription();
    }

    public void update(GroupCreateOrUpdateCmd cmd, LDAPProperties ldapProperties) {
        this.name = cmd.getName();
        this.ldapProperties = ldapProperties;
        this.members = this.buildListMember(this.getMembers());
        this.description = cmd.getDescription();
    }

    public void addMember(UserLdap userLdap) {
        String Cn = userLdap.getId().toString() + StringPool.COMMA + this.ldapProperties.getBase();
        this.getMembers().forEach(s -> {
            if (s.toUpperCase().startsWith(userLdap.getId().toString().toUpperCase())) {
                throw new ResponseException(BadRequestError.USER_IS_ALREADY_IN_GROUP);
            }
        });
        this.getMembers().add(Cn);
    }

    public Name buildDn(String name) {
        return LdapNameBuilder.newInstance()
                .add(Const.COMMON_NAME, name).build();
    }

    public String buildDnMember(String name) {
        return Const.COMMON_NAME + StringPool.EQUAL + name + StringPool.COMMA + this.ldapProperties.getBase();
    }

    public List<String> buildListMember(List<String> members) {
        return ObjectUtils.isNotEmpty(members) ? members.stream().map(member -> buildDnMember(member)).collect(Collectors.toList()) : new ArrayList<>();
    }
}
