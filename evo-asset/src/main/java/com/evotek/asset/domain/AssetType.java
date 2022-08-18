package com.evotek.asset.domain;


import com.evotek.asset.domain.command.AssetTypeCreateCmd;
import com.evotek.asset.domain.command.AssetTypeUpdateCmd;
import com.evotek.asset.infrastructure.support.enums.AssetTypeStatus;
import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.common.validator.ValidateConstraint;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import javax.persistence.Column;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Id;
import java.util.Objects;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class AssetType extends AuditableDomain {
    private String id;
    private String code;
    private String name;
    private AssetTypeStatus status;
    private String description;
    private Boolean deleted;

    public AssetType(AssetTypeCreateCmd cmd) {
        this.id = IdUtils.nextId();
        this.code = cmd.getCode();
        this.name = cmd.getName();
        this.status = Objects.nonNull(cmd.getStatus()) ? cmd.getStatus() : AssetTypeStatus.ACTIVE;
        this.description = cmd.getDescription();
        this.deleted = Boolean.FALSE;
    }

    public void updateAssetType(AssetTypeUpdateCmd cmd) {
        this.name = cmd.getName();
        this.description = cmd.getDescription();
    }

    public void active() {
        this.status = AssetTypeStatus.ACTIVE;
    }

    public void inactive() {
        this.status = AssetTypeStatus.INACTIVE;
    }

    public void delete() {
        this.deleted = Boolean.TRUE;
    }

}
