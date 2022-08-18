package com.evotek.asset.domain.query;

import com.evotek.asset.infrastructure.support.enums.AssetStatus;
import com.evotek.common.query.PagingQuery;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class AssetSearchQuery extends PagingQuery {
    private AssetStatus status;
}
