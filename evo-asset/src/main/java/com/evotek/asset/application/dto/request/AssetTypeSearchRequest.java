package com.evotek.asset.application.dto.request;

import com.evotek.asset.infrastructure.support.enums.AssetTypeStatus;
import com.evotek.common.dto.request.PagingRequest;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
public class AssetTypeSearchRequest extends PagingRequest {
    private String keyword;
    private AssetTypeStatus status;
}
