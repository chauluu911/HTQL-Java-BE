package com.evotek.iam.domain.query;

import com.evotek.common.query.PagingQuery;
import com.evotek.common.query.Query;
import com.evotek.iam.infrastructure.support.enums.JobTitleStatus;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

import java.time.Instant;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class JobTitleSearchQuery extends PagingQuery {
    private JobTitleStatus status;
    private String keyword;
    private Instant startCreatedAt;
    private Instant startLastModifiedAt;
}
