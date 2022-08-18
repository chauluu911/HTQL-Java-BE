package com.evotek.iam.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.enums.Scope;
import com.evotek.common.util.IdUtils;
import lombok.*;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class Permission extends AuditableDomain {

    private String id;
    private Scope scope;
    private String resourceCode;
    private String resourceName;
    private String name;
    private Boolean deleted;
    private Integer priority;

    public Permission(String resourceCode, Scope scope, String name, Integer priority) {
        this.id = IdUtils.nextId();
        this.resourceCode = resourceCode;
        this.scope = scope;
        this.name = name;
        this.priority = priority;
        this.deleted = false;
    }

    public void deleted() {
        this.deleted = true;
    }

    public void enrichResourceName(String resourceName) {
        this.resourceName = resourceName;
    }
}
