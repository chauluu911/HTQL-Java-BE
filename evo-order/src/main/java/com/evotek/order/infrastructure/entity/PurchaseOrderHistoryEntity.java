package com.evotek.order.infrastructure.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.order.infrastructure.support.enums.PurchaseOrderStatus;
import org.hibernate.Hibernate;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Objects;

@Entity
@Table(name = "purchase_order_history")
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class PurchaseOrderHistoryEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "created_user_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String createdUserId;

    @Column(name = "purchase_order_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String purchaseOrderId;

    @Column(name = "status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    private PurchaseOrderStatus status;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        PurchaseOrderHistoryEntity that = (PurchaseOrderHistoryEntity) o;
        return id != null && Objects.equals(id, that.id);
    }
}
