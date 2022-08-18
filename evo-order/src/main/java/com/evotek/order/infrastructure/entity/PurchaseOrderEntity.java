package com.evotek.order.infrastructure.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.order.infrastructure.support.enums.PurchaseOrderStatus;
import com.evotek.order.infrastructure.support.enums.PurchaseOrderType;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.Hibernate;
import org.hibernate.validator.constraints.Range;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Enumerated;
import javax.persistence.EnumType;
import java.math.BigDecimal;
import java.time.Instant;
import java.util.Objects;

@Entity
@Table(name = "purchase_order")
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class PurchaseOrderEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "created_user_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String createdUserId;

    @Column(name = "owner_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String ownerId;

    @Column(name = "code", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, nullable = false)
    private String code;

    @Column(name = "menu_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String menuId;

    @Column(name = "total_price", nullable = false)
    @Range(min = 0)
    private BigDecimal totalPrice;

    @Column(name = "total_quantity", nullable = false)
    @Range(min = 1)
    private Integer totalQuantity;

    @Column(name = "status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private PurchaseOrderStatus status;

    @Column(name = "type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private PurchaseOrderType type;

    @Column(name = "payment_at")
    private Instant paymentAt;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        PurchaseOrderEntity that = (PurchaseOrderEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }


}
