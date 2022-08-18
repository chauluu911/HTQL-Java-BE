package com.evotek.asset.domain.repository;

import com.evotek.asset.domain.AssetHistory;
import com.evotek.common.webapp.support.DomainRepository;

import java.util.List;

public interface AssetHistoryDomainRepository extends DomainRepository<AssetHistory, String> {
    public List<AssetHistory> getAllByAssetId(String id);
}
