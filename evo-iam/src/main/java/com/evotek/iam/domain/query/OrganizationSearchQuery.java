package com.evotek.iam.domain.query;

import com.evotek.common.query.PagingQuery;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class OrganizationSearchQuery extends PagingQuery {
    private String keyword;
    private String status;
    private LocalDate incorporationDate;
}

