package com.evotek.asset.domain;

import com.evotek.asset.application.dto.request.AssetCreateRequest;
import com.evotek.asset.domain.command.AssetCreateCmd;
import com.evotek.asset.domain.command.AssetUpdateCmd;
import com.evotek.asset.infrastructure.support.enums.AssetStatus;
import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.w3c.dom.stylesheets.LinkStyle;

import java.util.List;
import java.util.Objects;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class Asset extends AuditableDomain {
    private String id;
    private String code;
    private String name;
    private String serialNumber;
    private AssetStatus status;
    private String description;
    private String productId;
    private String purchaseOrderId;
    private String assetTypeId;
    private Integer total;
    private List<AssetHistory> assetHistories;
    private AssetType assetType;
    private Boolean deleted;

    public Asset(AssetCreateCmd cmd) {
        this.id = IdUtils.nextId();
        this.name = cmd.getName();
        this.serialNumber = cmd.getSerialNumber();
        this.status = cmd.getStatus();
        this.description = cmd.getDescription();
        this.productId = cmd.getProductId();
        this.purchaseOrderId = cmd.getPurchaseOrderId();
        this.assetTypeId = cmd.getAssetTypeId();
        this.total = cmd.getTotal();
        this.deleted = false;
    }

    public void updateAsset(AssetUpdateCmd cmd) {
        this.name = cmd.getName();
        this.serialNumber = cmd.getSerialNumber();
        this.status = cmd.getStatus();
        this.description = cmd.getDescription();
        this.total = cmd.getTotal();
        this.assetTypeId = cmd.getAssetTypeId();
    }

    public void enrichCode(String code) {
        this.code = code;
    }

    public void enrichAssetHistory(List<AssetHistory> assetHistories) {
        this.assetHistories = assetHistories;
    }

    public void enrichAssetType(AssetType assetType) {
        this.assetType = assetType;
    }

    public void delete() {
        this.deleted = true;
    }
}
