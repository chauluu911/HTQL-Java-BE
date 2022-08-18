package com.evotek.asset.domain.query;


import com.evotek.asset.infrastructure.support.enums.AssetTypeStatus;
import com.evotek.common.query.PagingQuery;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class AssetTypeSearchQuery extends PagingQuery {
    private AssetTypeStatus status;
}
