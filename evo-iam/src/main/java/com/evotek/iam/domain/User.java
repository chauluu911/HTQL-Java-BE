package com.evotek.iam.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.enums.AccountType;
import com.evotek.common.enums.Gender;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.ldapcore.dto.response.UserLdapResponse;
import com.evotek.common.util.DataUtil;
import com.evotek.common.util.IdUtils;
import com.evotek.common.util.StringPool;
import com.evotek.iam.domain.command.*;
import com.evotek.iam.infrastructure.support.enums.AuthenticationType;
import com.evotek.iam.infrastructure.support.enums.EmploymentStatus;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.springframework.util.CollectionUtils;

import java.time.Instant;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class User extends AuditableDomain {
    private String id;
    private String username;
    @JsonIgnore
    private String password;
    private String fullName;
    private String email;
    private String phoneNumber;
    private LocalDate dayOfBirth;
    private Gender gender;
    private List<Role> roles = new ArrayList<>();
    private Boolean deleted;
    private AuthenticationType authenticationType;
    private String employeeCode;
    private String description;
    private UserStatus status;
    private String avatarFileId;
    private String avatarFileUrl;
    private AccountType accountType;
    private Instant lastAuthChangeAt;
    private String departmentId;
    private String departmentName;
    private Department department;
    private String jobTitleId;
    private String jobTitleName;
    private List<EmploymentHistory> employmentHistories;
    @JsonIgnore
    private List<UserRole> userRoles;

    public User(ImportUserCmd cmd) {
        this.id = IdUtils.nextId();
        this.fullName = cmd.getFullName();
        this.email = cmd.getEmail();
        this.phoneNumber = cmd.getPhoneNumber();
        this.dayOfBirth = cmd.getDayOfBirth();
        this.employeeCode = cmd.getEmployeeCode();
        this.gender = cmd.getGender() != null ? cmd.getGender() : Gender.OTHER;
        this.status = cmd.getStatus() != null ? cmd.getStatus() : UserStatus.ACTIVE;
        this.accountType = AccountType.EMPLOYEE;
        this.username = cmd.getUsername();
        this.authenticationType = Objects.nonNull(cmd.getAuthenticationType()) ? cmd.getAuthenticationType() : AuthenticationType.LDAP;
        this.jobTitleId = cmd.getJobTitleId();
        this.deleted = false;
    }

    public User(UserInternalCreateCmd cmd, List<Role> existedRoles) {
        this.id = IdUtils.nextId();
        this.username = cmd.getUsername();
        this.fullName = cmd.getFullName();
        this.email = cmd.getEmail();
        this.phoneNumber = cmd.getPhoneNumber();
        this.dayOfBirth = cmd.getDayOfBirth();
        this.description = cmd.getDescription();
        this.employeeCode = cmd.getEmployeeCode();
        this.avatarFileId = cmd.getAvatarFileId();
        this.status = cmd.getStatus() != null ? cmd.getStatus() : UserStatus.ACTIVE;
        this.gender = cmd.getGender() != null ? cmd.getGender() : Gender.OTHER;
        this.authenticationType = Objects.nonNull(cmd.getAuthenticationType()) ? cmd.getAuthenticationType() : AuthenticationType.INTERNAL;
        this.accountType = AccountType.EMPLOYEE;
        this.password = DataUtil.getValueOrDefault(cmd.getPassword(), StringPool.BLANK);
        this.jobTitleId = cmd.getJobTitleId();

        // add departmentUser
        if (CollectionUtils.isEmpty(this.employmentHistories)) {
            this.employmentHistories = new ArrayList<>();
        }
        if (Objects.nonNull(cmd.getEmployment())) {
            this.departmentId = cmd.getEmployment().getDepartmentId();
            this.employmentHistories.add(new EmploymentHistory(this.id, cmd.getEmployment()));
        }
        this.departmentId = cmd.getDepartmentId();
        assignRoles(cmd.getRoleIds(), existedRoles);
        this.deleted = false;
    }

    public User(UserLdapResponse res) {
        this.id = IdUtils.nextId();
        this.username = res.getUserName();
        this.fullName = Objects.nonNull(res.getDisplayName()) ? res.getDisplayName() : res.getUserName();
        this.email = res.getUserPrincipalName();
        this.phoneNumber = res.getTelephoneNumber();
        this.password = res.getMail();
        this.gender = Gender.OTHER;
        this.authenticationType = AuthenticationType.LDAP;
        this.status = UserStatus.INACTIVE;
        this.accountType = AccountType.EMPLOYEE;
        this.deleted = false;
    }

    public User(UserRegisterCmd cmd) {
        this.id = IdUtils.nextId();
        this.username = cmd.getUsername();
        this.password = cmd.getPassword();
        this.fullName = cmd.getFullName();
        this.email = cmd.getEmail();
        this.phoneNumber = cmd.getPhoneNumber();
        this.authenticationType = AuthenticationType.INTERNAL;
        this.status = UserStatus.ACTIVE;
        this.gender = cmd.getGender() != null ? cmd.getGender() : Gender.OTHER;
        this.accountType = AccountType.EMPLOYEE;
        this.jobTitleId = cmd.getJobTitleId();
        this.deleted = false;
    }

    public User(String username, String password, String fullName, String roleId) {
        this.id = IdUtils.nextId();
        this.username = username;
        this.password = password;
        this.fullName = fullName;
        this.status = UserStatus.ACTIVE;
        this.gender = Gender.OTHER;
        this.authenticationType = AuthenticationType.INTERNAL;
        this.accountType = AccountType.EMPLOYEE;
        this.userRoles = new ArrayList<>();
        this.userRoles.add(new UserRole(this.id, roleId));
        this.deleted = false;
    }


    public void updateProfile(UserUpdateProfileCmd cmd) {
        this.fullName = cmd.getFullName();
        this.email = cmd.getEmail();
        this.phoneNumber = cmd.getPhoneNumber();
        this.dayOfBirth = cmd.getDayOfBirth();
        this.gender = cmd.getGender();
        this.avatarFileId = cmd.getAvatarFileId();
        this.jobTitleId = cmd.getJobTitleId();
    }

    public void changePassword(String newPassword) {
        if (AuthenticationType.LDAP.equals(this.authenticationType)) {
            throw new ResponseException(BadRequestError.CHANGE_PASSWORD_NOT_SUPPORTED);
        }
        this.password = newPassword;
        this.lastAuthChangeAt = Instant.now();
    }

    public void update(UserUpdateCmd cmd, List<Role> existedRoles) {
        this.fullName = cmd.getFullName();
        this.email = cmd.getEmail();
        this.phoneNumber = cmd.getPhoneNumber();
        this.dayOfBirth = cmd.getDayOfBirth();
        this.gender = Objects.isNull(cmd.getGender()) ? this.gender : cmd.getGender();
        this.status = cmd.getStatus();
        this.description = cmd.getDescription();
        this.employeeCode = cmd.getEmployeeCode();
        this.avatarFileId = cmd.getAvatarFileId();
        this.jobTitleId = cmd.getJobTitleId();
        this.roles = this.roles == null ? new ArrayList<>() : this.roles;

        if (Objects.nonNull(cmd.getEmployment())) {
            if (CollectionUtils.isEmpty(this.employmentHistories)) {
                this.employmentHistories = new ArrayList<>();
            }
            if (!Objects.equals(cmd.getEmployment().getDepartmentId(), this.departmentId)
                    || !Objects.equals(cmd.getEmployment().getJobTitleId(), this.getJobTitleId())) {
                // check add new department user
                this.employmentHistories.forEach(EmploymentHistory::leaveDepartment);
                this.departmentId = cmd.getEmployment().getDepartmentId();
                this.employmentHistories.add(new EmploymentHistory(this.id, cmd.getEmployment()));
            } else {
                // update department user
                this.currentEmployment().ifPresent(e -> e.update(cmd.getEmployment()));
            }
        }

        if (CollectionUtils.isEmpty(this.userRoles)) {
            this.userRoles = new ArrayList<>();
        } else {
            this.userRoles.forEach(UserRole::deleted);
        }

        if (CollectionUtils.isEmpty(existedRoles)) {
            existedRoles = new ArrayList<>();
        }

        List<String> assignedRoleIds;
        if (CollectionUtils.isEmpty(cmd.getRoleIds())) {
            assignedRoleIds = new ArrayList<>();
        } else {
            assignedRoleIds = cmd.getRoleIds().stream().distinct().collect(Collectors.toList());
        }

        for (String roleId : assignedRoleIds) {
            Optional<Role> optionalRole = existedRoles.stream().filter(r -> r.getId().equals(roleId)).findFirst();
            if (optionalRole.isEmpty()) {
                throw new ResponseException(BadRequestError.ROLE_INVALID);
            }

            Optional<UserRole> optionalUserRole = this.userRoles.stream()
                    .filter(item -> item.getRoleId().equals(roleId)).findFirst();
            if (optionalUserRole.isEmpty()) {
                this.userRoles.add(new UserRole(this.id, roleId));
            } else {
                UserRole userRole = optionalUserRole.get();
                userRole.unDelete();
            }
            optionalRole.ifPresent(role -> this.roles.add(role));
        }
        this.deleted = false;
    }

    public void deleted() {
        if (UserStatus.ACTIVE.equals(this.status)) {
            throw new ResponseException(BadRequestError.CANNOT_DELETE_ACTIVE_USER);
        }
        if (Boolean.TRUE.equals(this.deleted)) {
            throw new ResponseException(BadRequestError.CANNOT_DELETE_DELETED_USER);
        }
        this.deleted = true;
    }

    public void updateImport(ImportUserCmd importUserCmd) {
        this.fullName = importUserCmd.getFullName();
        this.phoneNumber = importUserCmd.getPhoneNumber();
        this.dayOfBirth = importUserCmd.getDayOfBirth();
        this.employeeCode = importUserCmd.getEmployeeCode();
        this.gender = importUserCmd.getGender() != null ? importUserCmd.getGender() : Gender.OTHER;
        this.deleted = false;
    }

    private void assignRoles(List<String> roleIds, List<Role> existedRoles) {
        this.userRoles = new ArrayList<>();
        if (CollectionUtils.isEmpty(existedRoles)) {
            existedRoles = new ArrayList<>();
        }
        List<String> assignedRoleIds;
        if (CollectionUtils.isEmpty(roleIds)) {
            assignedRoleIds = new ArrayList<>();
        } else {
            assignedRoleIds = roleIds.stream().distinct().collect(Collectors.toList());
        }
        for (String roleId : assignedRoleIds) {
            Optional<Role> optionalRole = existedRoles.stream().filter(r -> r.getId().equals(roleId)).findFirst();
            if (optionalRole.isPresent()) {
                Role role = optionalRole.get();
                this.userRoles.add(new UserRole(this.id, role.getId()));
                this.roles.add(role);
            } else {
                throw new ResponseException(BadRequestError.ROLE_INVALID);
            }
        }
    }

    @JsonProperty("employment")
    public Optional<EmploymentHistory> currentEmployment() {
        if (CollectionUtils.isEmpty(this.employmentHistories)) {
            return Optional.empty();
        }
        return this.employmentHistories.stream()
                .filter(item -> Objects.equals(EmploymentStatus.ACTIVE, item.getStatus())
                        && Objects.equals(this.departmentId, item.getDepartmentId()))
                .findFirst();
    }

    public void enrichViewUrlFile(String avatarFileUrl) {
        this.avatarFileUrl = avatarFileUrl;
    }

    public void enrichRoles(List<Role> roles) {
        this.roles = roles;
    }

    public void enrichUserRoles(List<UserRole> userRoles) {
        this.userRoles = userRoles;
    }

    public void enrichDepartment(Department department) {
        this.department = department;
    }

    public void enrichEmploymentHistory(List<EmploymentHistory> employmentHistories) {
        this.employmentHistories = employmentHistories;
    }

    public void active() {
        this.status = UserStatus.ACTIVE;
        this.lastAuthChangeAt = null;
    }

    public void inactive() {
        this.status = UserStatus.INACTIVE;
        this.lastAuthChangeAt = Instant.now();
    }

    public void enrichDepartmentName(String departmentName) {
        this.departmentName = departmentName;
    }

    public void enrichJobTitleName(String jobTitleName) {
        this.jobTitleName = jobTitleName;
    }
}
