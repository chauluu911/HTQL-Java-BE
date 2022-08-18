package com.evotek.notification.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import lombok.*;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "user_telegram", indexes = {
        @Index(name = "user_telegram_user_id_idx", columnList = "user_id"),
})
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class UserTelegramEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "user_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String userId;

    @Column(name = "chat_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String chatId;

    @Column(name = "deleted")
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        UserTelegramEntity that = (UserTelegramEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
