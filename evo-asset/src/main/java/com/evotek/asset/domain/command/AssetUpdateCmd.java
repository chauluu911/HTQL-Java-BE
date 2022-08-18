package com.evotek.asset.domain.command;

import com.evotek.asset.infrastructure.support.enums.AssetStatus;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class AssetUpdateCmd {
    private String name;
    private String serialNumber;
    private AssetStatus status;
    private String description;
    private String assetTypeId;
    private String ownerId;
    private Integer total;
}
