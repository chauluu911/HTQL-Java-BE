package com.evotek.iam.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.enums.AccountType;
import com.evotek.common.enums.Gender;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.infrastructure.support.enums.AuthenticationType;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.time.Instant;
import java.time.LocalDate;
import java.util.Objects;

@Entity
@Table(name = "users", indexes = {
        @Index(name = "user_username_idx", columnList = "username"),
        @Index(name = "user_deleted_idx", columnList = "deleted")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class UserEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "username", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, nullable = false)
    private String username;

    @Column(name = "password", length = ValidateConstraint.LENGTH.VALUE_MAX_LENGTH)
    private String password;

    @Column(name = "full_name", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, nullable = false)
    private String fullName;

    @Column(name = "email", length = ValidateConstraint.LENGTH.EMAIL_MAX_LENGTH, nullable = false)
    private String email;

    @Column(name = "phone_number", length = ValidateConstraint.LENGTH.PHONE_MAX_LENGTH)
    private String phoneNumber;

    @Column(name = "day_of_birth")
    private LocalDate dayOfBirth;

    @Column(name = "gender", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private Gender gender = Gender.OTHER;

    @Column(name = "authentication_type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private AuthenticationType authenticationType = AuthenticationType.INTERNAL;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Column(name = "employee_code", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH)
    private String employeeCode;

    @Column(name = "title", length = ValidateConstraint.LENGTH.TITLE_MAX_LENGTH)
    private String title;

    @Column(name = "department_name", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH)
    private String departmentName;

    @Column(name = "description", length = ValidateConstraint.LENGTH.DESC_MAX_LENGTH)
    private String description;

    @Column(name = "status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private UserStatus status = UserStatus.ACTIVE;

    @Column(name = "avatar_file_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String avatarFileId;

    @Column(name = "account_type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH)
    @Enumerated(EnumType.STRING)
    private AccountType accountType;

    @Column(name = "last_auth_change_at")
    private Instant lastAuthChangeAt;

    @Column(name = "department_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String departmentId;

    @Column(name = "account_telegram", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH)
    private String accountTelegram;

    @Column(name = "job_title_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String jobTitleId;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        UserEntity that = (UserEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
