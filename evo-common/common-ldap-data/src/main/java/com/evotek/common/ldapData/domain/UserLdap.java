package com.evotek.common.ldapData.domain;


import com.evotek.common.ldapData.domain.command.UserCreateOrUpdateCmd;
import com.evotek.common.ldapData.infrastructure.support.util.Const;
import com.evotek.common.ldapcore.config.LDAPProperties;
import com.evotek.common.util.StringPool;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import org.springframework.ldap.support.LdapNameBuilder;

import javax.naming.Name;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Setter
@ToString
@NoArgsConstructor
public class UserLdap {
    // build by fullName
    @JsonIgnore
    private Name id;
    private String fullName;
    private String lastName;
    private String uid;
    private String username;
    private String userPrincipalName;
    @JsonIgnore
    private List<String> memberOf;
    @JsonIgnore
    private LDAPProperties ldapProperties;


    public UserLdap(UserCreateOrUpdateCmd cmd, LDAPProperties ldapProperties) {
        this.id = this.buildName(cmd.getFullName());
        this.username = cmd.getUserName();
        this.fullName = cmd.getFullName();
        this.lastName = cmd.getFullName();
        this.uid = cmd.getFullName();
        this.ldapProperties = ldapProperties;
        this.userPrincipalName = cmd.getUserName() + StringPool.AT + this.ldapProperties.getDomain();
        this.memberOf = cmd.getMemberOf().stream().map(s -> buildDnMember(s)).collect(Collectors.toList());
    }

    public void update(UserCreateOrUpdateCmd cmd, LDAPProperties ldapProperties) {
        this.username = cmd.getUserName();
        this.fullName = cmd.getFullName();
        this.lastName = cmd.getFullName();
        this.uid = cmd.getFullName();
        this.ldapProperties = ldapProperties;
        this.userPrincipalName = cmd.getUserName() + StringPool.AT + this.ldapProperties.getDomain();
        this.memberOf = cmd.getMemberOf().stream().map(s -> buildDnMember(s)).collect(Collectors.toList());
    }

    public Name buildName(String id) {
        return LdapNameBuilder.newInstance()
                .add(Const.COMMON_NAME, id).build();
    }

    public String buildDnMember(String name) {
        return "CN= " + name + StringPool.COMMA + this.ldapProperties.getBase();
    }
}
