package com.evotek.asset.domain;

import com.evotek.asset.infrastructure.support.enums.AssetStatus;
import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class AssetHistory extends AuditableDomain {
    private String id;
    private String assetId;
    private String ownerId;
    private String description;
    private AssetStatus status;
    private Boolean deleted;

    public AssetHistory(Asset asset, String ownerId) {
        this.id = IdUtils.nextId();
        this.assetId = asset.getId();
        this.status = asset.getStatus();
        this.ownerId = ownerId;
        this.description = asset.getDescription();
        this.deleted = false;
    }

}
