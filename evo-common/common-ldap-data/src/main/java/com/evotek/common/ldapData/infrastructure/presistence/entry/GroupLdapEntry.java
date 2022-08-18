package com.evotek.common.ldapData.infrastructure.presistence.entry;

import com.evotek.common.ldapData.infrastructure.support.util.Const;
import lombok.*;
import org.springframework.data.domain.Persistable;
import org.springframework.ldap.odm.annotations.*;

import javax.naming.Name;
import java.util.List;

@Entry(objectClasses = {Const.OBJECT_CLASS_TOP, Const.OBJECT_CLASS_GROUP})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class GroupLdapEntry implements Persistable {
    @Id
    private Name id;

    @Attribute(name = "cn")
    @DnAttribute("cn")
    private String name;

    @Attribute(name = "member")
    private List<String> members;

    @Attribute(name = "description")
    private String description;

    @Transient
    private boolean isNew;

    @Override
    public boolean isNew() {
        return this.isNew;
    }

}
