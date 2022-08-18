package com.evotek.common.ldapData.infrastructure.presistence.entry;

import com.evotek.common.ldapData.infrastructure.support.util.Const;
import lombok.*;
import org.springframework.data.domain.Persistable;
import org.springframework.ldap.odm.annotations.Attribute;
import org.springframework.ldap.odm.annotations.Entry;
import org.springframework.ldap.odm.annotations.Id;
import org.springframework.ldap.odm.annotations.Transient;

import javax.naming.Name;
import java.util.List;

@Entry(
        objectClasses = {Const.OBJECT_CLASS_TOP, Const.OBJECT_CLASS_USER, Const.OBJECT_CLASS_ORGANIZATIONAL_PERSON, Const.OBJECT_CLASS_PERSON}
)
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class UserLdapEntry implements Persistable {
    @Id
    private Name id;
    private @Attribute(name = "cn")
    String fullName;
    private @Attribute(name = "sn")
    String lastName;
    private @Attribute(name = "uid")
    String uid;
    private @Attribute(name = "sAMAccountName")
    String username;
    private @Attribute(name = "userPrincipalName")
    String userPrincipalName;
    private @Attribute(name = "memberOf")
    List<String> memberOf;

    @Transient
    private boolean isNew;

    @Override
    public boolean isNew() {
        return this.isNew;
    }
}
