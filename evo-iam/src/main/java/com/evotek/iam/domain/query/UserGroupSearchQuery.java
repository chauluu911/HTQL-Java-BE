package com.evotek.iam.domain.query;

import com.evotek.common.query.PagingQuery;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class UserGroupSearchQuery extends PagingQuery {
    private String keyword;
    private String type;
}
