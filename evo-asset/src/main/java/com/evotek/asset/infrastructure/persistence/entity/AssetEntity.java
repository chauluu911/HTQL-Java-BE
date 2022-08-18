package com.evotek.asset.infrastructure.persistence.entity;

import com.evotek.asset.infrastructure.support.enums.AssetStatus;
import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.hibernate.Hibernate;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "asset", indexes = {
        @Index(name = "asset_id_idx", columnList = "id"),
        @Index(name = "asset_code_idx", columnList = "code"),
        @Index(name = "asset_deleted_idx", columnList = "deleted")
})
@Data
@RequiredArgsConstructor
@AllArgsConstructor
public class AssetEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "code", length = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, nullable = false)
    private String code;

    @Column(name = "serial_number", length = ValidateConstraint.LENGTH.SERIAL_NUMBER_MAX_LENGTH)
    private String serialNumber;

    @Column(name = "name", length = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, nullable = false)
    private String name;

    @Column(name = "status", length = ValidateConstraint.LENGTH.ENUM_MAX_LENGTH, nullable = false)
    @Enumerated(EnumType.STRING)
    private AssetStatus status;

    @Column(name = "description", length = ValidateConstraint.LENGTH.DESC_MAX_LENGTH)
    private String description;

    @Column(name = "product_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String productId;

    @Column(name = "purchase_order_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String purchaseOrderId;

    @Column(name = "asset_type_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String assetTypeId;

    @Column(name = "total")
    private Integer total;

    @Column(name = "deleted")
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        AssetEntity that = (AssetEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
