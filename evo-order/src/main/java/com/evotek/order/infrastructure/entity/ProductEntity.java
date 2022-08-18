package com.evotek.order.infrastructure.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.order.infrastructure.support.enums.ProductSubType;
import com.evotek.order.infrastructure.support.enums.ProductType;
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
import java.math.BigDecimal;
import java.util.Objects;

@Entity
@Table(name = "product")
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class ProductEntity extends AuditableEntity {
    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "name", length = ValidateConstraint.LENGTH.PRODUCT_NAME_MAX_LENGTH, nullable = false)
    private String name;

    @Column(name = "code", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, nullable = false)
    private String code;

    @Column(name = "price", nullable = false)
    @Range(min = 0)
    private BigDecimal price;

    @Column(name = "type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    private ProductType type;

    @Column(name = "sub_type", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH)
    private ProductSubType subType;

    @Column(name = "image_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String imageId;

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
        ProductEntity that = (ProductEntity) o;
        return id != null && Objects.equals(id, that.id);
    }
}
