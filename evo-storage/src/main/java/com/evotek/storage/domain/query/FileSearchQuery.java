package com.evotek.storage.domain.query;

import com.evotek.common.query.PagingQuery;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class FileSearchQuery extends PagingQuery {

    private String keyword;

    private String ownerId;

    private String ownerType;
}
