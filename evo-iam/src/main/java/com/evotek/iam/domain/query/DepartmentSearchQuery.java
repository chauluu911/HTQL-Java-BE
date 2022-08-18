package com.evotek.iam.domain.query;

import com.evotek.common.query.PagingQuery;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

import java.time.Instant;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class DepartmentSearchQuery extends PagingQuery {
    private String keyword;

    private Instant startAt;

    private Instant endAt;

    private String status;

    private Instant startCreatedAt;

    private Instant endCreatedAt;

    private Instant startLastModifiedAt;

    private Instant endLastModifiedAt;
}
