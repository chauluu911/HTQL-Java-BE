package com.evotek.asset.infrastructure.persistence.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;

import javax.persistence.*;
import java.time.LocalDate;

@Entity
@Table(name = "asset_history", indexes = {
        @Index(name = "asset_history_deleted_ids", columnList = "deleted")
})
@Data
public class AssetHistoryEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "asset_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String assetId;

    @Column(name = "owner_id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String ownerId;

    @Column(name = "status", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH)
    private String status;

    @Column(name = "description", length = ValidateConstraint.LENGTH.DESC_MAX_LENGTH, nullable = false)
    private String description;

    @Column(name = "deleted")
    private Boolean deleted;

}
