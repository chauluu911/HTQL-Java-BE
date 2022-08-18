package com.evotek.iam.infrastructure.support.enums;

import com.evotek.common.enums.Scope;

import java.util.List;

public enum ResourceCategory {
    USER_MANAGEMENT("USER", "USER_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.VIEW, Scope.UPDATE), 5),
    ROLE_MANAGEMENT("ROLE", "ROLE_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.VIEW, Scope.UPDATE), 6),
    NOTIFICATION_MANAGEMENT("NOTIFICATION", "NOTIFICATION_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.VIEW, Scope.UPDATE, Scope.DELETE), 3),
    FILE_MANAGEMENT("FILE", "FILE_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.VIEW), 4),
    MEETING_MANAGEMENT("MEETING", "MEETING_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.VIEW, Scope.UPDATE), 1),
    PURCHASE_ORDER_MANAGEMENT("ORDER", "PURCHASE_ORDER_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.VIEW, Scope.UPDATE), 2),
    ROOM_MANAGEMENT("ROOM", "ROOM_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.VIEW, Scope.UPDATE), 7),
    DEPARTMENT_MANAGEMENT("DEPARTMENT", "DEPARTMENT_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.UPDATE, Scope.VIEW, Scope.DELETE), 8),
    EMPLOYEE_MANAGEMENT("EMPLOYEE", "EMPLOYEE_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.UPDATE, Scope.VIEW), 9),
    CLIENT_MANAGEMENT("CLIENT", "CLIENT_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.VIEW, Scope.UPDATE), 10),
    ORGANIZATION_MANAGEMENT("ORGANIZATION", "ORGANIZATION_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.UPDATE, Scope.VIEW), 11),
    GROUP_USER_MANAGEMENT("USER_GROUP", "GROUP_USER_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.VIEW, Scope.UPDATE, Scope.DELETE), 12),
    JOB_TITLE_MANAGEMENT("JOB_TITLE", "JOB_TITLE_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.VIEW, Scope.UPDATE, Scope.DELETE), 13),
    ASSET_MANAGEMENT("ASSET", "ASSET_MANAGEMENT_TITLE", List.of(Scope.CREATE, Scope.UPDATE, Scope.VIEW, Scope.DELETE), 14);
    String resourceCode;
    String resourceName;
    Integer priority;
    List<Scope> scopes;

    ResourceCategory(String resourceCode, String resourceName, List<Scope> scopes, Integer priority) {
        this.resourceCode = resourceCode;
        this.resourceName = resourceName;
        this.scopes = scopes;
        this.priority = priority;
    }

    public String getResourceCode() {
        return resourceCode;
    }

    public String getResourceName() {
        return resourceName;
    }

    public List<Scope> getScopes() {
        return scopes;
    }

    public Integer getPriority() {
        return priority;
    }
}
