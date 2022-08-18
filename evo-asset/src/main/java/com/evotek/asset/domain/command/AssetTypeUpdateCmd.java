package com.evotek.asset.domain.command;

import lombok.Data;

@Data
public class AssetTypeUpdateCmd {
    private String name;
    private String description;
}
