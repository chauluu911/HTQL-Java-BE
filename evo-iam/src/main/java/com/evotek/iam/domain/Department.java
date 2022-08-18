package com.evotek.iam.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.IdUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.common.util.StringPool;
import com.evotek.iam.domain.command.DepartmentCreateCmd;
import com.evotek.iam.domain.command.DepartmentUpdateCmd;
import com.evotek.iam.infrastructure.support.enums.DepartmentStatus;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Objects;


@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class Department extends AuditableDomain {
    private String id;
    private String name;
    private String code;
    private String parentId;
    private String description;
    private Boolean deleted;
    private String parentPath;
    private DepartmentStatus status;
    private Department parent;
    private List<Department> children;
    private List<EmploymentHistory> employmentHistories;

    public Department(DepartmentCreateCmd cmd, Department parent) {
        this.id = IdUtils.nextId();
        this.code = cmd.getCode();
        this.name = cmd.getName();
        this.parentId = cmd.getParentId();
        this.parent = parent;
        this.description = cmd.getDescription();
        this.deleted = false;
        this.status = DepartmentStatus.ACTIVE;
        if (Objects.nonNull(parent)) {
            this.parentPath = parent.getPath();
        } else {
            this.parentPath = null;
        }
    }

    public void update(DepartmentUpdateCmd cmd, Department newParent) {
        if (Objects.nonNull(newParent)) {
            this.name = cmd.getName();
            this.description = cmd.getDescription();
            this.deleted = false;
            //check update khong duoc ha cap
            if (newParent.getPath().contains(this.getPath())) {
                throw new ResponseException(BadRequestError.UPDATE_DEPARTMENT_NOT_SUPPORTED);
            }
            this.parentId = cmd.getParentId();
            this.parent = newParent;
            updateParentPath(newParent.getPath());
        } else {
            this.name = cmd.getName();
            this.description = cmd.getDescription();
            this.deleted = false;
        }
    }

    public void delete() {
        this.deleted = true;
    }

    public void updateParentPath(String parentPath) {
        if (StrUtils.isBlank(parentPath)) {
            throw new ResponseException(BadRequestError.DEPARTMENT_NOT_EXITED);
        }
        this.parentPath = parentPath;

        if (!CollectionUtils.isEmpty(this.children)) {
            this.children.forEach(child -> child.enrichPath(this.getPath()));
        }
    }

    public void active() {
        this.status = DepartmentStatus.ACTIVE;
    }

    public void inactive() {
        this.status = DepartmentStatus.INACTIVE;
    }

    public String getPath() {
        if (StringUtils.hasLength(this.parentPath)) {
            return this.parentPath + StringPool.SLASH + this.id;
        } else {
            return this.id;
        }
    }

    public void enrichEmploymentHistory(List<EmploymentHistory> employmentHistories, List<User> users) {
        employmentHistories.forEach(employmentHistory -> {
            users.forEach(user -> {
                if (employmentHistory.getUserId().equals(user.getId())) {
                    employmentHistory.enrichFullName(user.getFullName());
                    return;
                }
            });
        });
        this.employmentHistories = employmentHistories;

    }

    public void enrichChildren(List<Department> departments) {
        this.children = departments;
    }

    public void enrichPath(String newParentPath) {
        this.parentPath = newParentPath;
    }
}
