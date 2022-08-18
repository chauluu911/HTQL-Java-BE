package com.evotek.order.infrastructure.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.order.infrastructure.support.enums.MenuType;
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
@Table(name = "menu")
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class MenuEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "code", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, nullable = false)
    private String code;

    @Column(name = "title", length = ValidateConstraint.LENGTH.TITLE_MAX_LENGTH, nullable = false)
    private String title;

    @Range(min = 0)
    @Column(name = "price", nullable = false)
    private BigDecimal price;

    @Range(min = 0)
    @Column(name = "max_total_price_purchase_order")
    private BigDecimal maxTotalPricePurchaseOrder;

    @Column(name = "closed_at")
    private Instant closedAt;

    @Column(name = "type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private MenuType type;

    @Column(name = "note", length = ValidateConstraint.LENGTH.NOTE_MAX_LENGTH)
    private String note;

    @Column(name = "published", nullable = false)
    private Boolean published;

    @Column(name = "deleted", nullable = false)
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        MenuEntity that = (MenuEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }


}
