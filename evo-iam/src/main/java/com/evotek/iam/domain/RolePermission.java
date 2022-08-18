package com.evotek.iam.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.enums.Scope;
import com.evotek.common.util.IdUtils;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.*;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class RolePermission extends AuditableDomain {

    @JsonIgnore
    private String id;
    private String roleId;
    private String resourceCode;
    private Scope scope;
    private Boolean deleted;

    public RolePermission(String roleId, String resourceCode, Scope scope) {
        this.id = IdUtils.nextId();
        this.roleId = roleId;
        this.resourceCode = resourceCode;
        this.scope = scope;
        this.deleted = false;
    }

    public void deleted() {
        this.deleted = true;
    }

    public void unDelete() {
        this.deleted = false;
    }
}
