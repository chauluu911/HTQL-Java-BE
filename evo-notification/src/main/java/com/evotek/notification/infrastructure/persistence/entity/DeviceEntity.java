package com.evotek.notification.infrastructure.persistence.entity;


import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.notification.infrastructure.support.enums.DeviceType;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "device", indexes = {
        @Index(name = "device_user_id_idx", columnList = "user_id"),
        @Index(name = "device_device_type_idx", columnList = "device_type")
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class DeviceEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "name", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH)
    private String name;

    @Column(name = "user_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String userId;

    @Column(name = "device_id", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, nullable = false)
    private String deviceId;

    @Column(name = "device_token", length = ValidateConstraint.LENGTH.VALUE_MAX_LENGTH, nullable = false)
    private String deviceToken;

    @Column(name = "device_type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private DeviceType deviceType;

    @Column(name = "device_info", columnDefinition = "text", length = ValidateConstraint.LENGTH.CONTENT_MAX_LENGTH, nullable = false)
    private String deviceInfo;

    @Column(name = "app_version", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, nullable = false)
    private String appVersion;

    @Column(name = "deleted")
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        DeviceEntity that = (DeviceEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
