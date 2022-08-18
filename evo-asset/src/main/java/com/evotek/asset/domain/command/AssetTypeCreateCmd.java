package com.evotek.asset.domain.command;

import com.evotek.asset.infrastructure.support.enums.AssetTypeStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AssetTypeCreateCmd {
    private String code;
    private String name;
    private AssetTypeStatus status;
    private String description;

}
