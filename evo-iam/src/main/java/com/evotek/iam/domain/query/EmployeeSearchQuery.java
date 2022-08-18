package com.evotek.iam.domain.query;

import com.evotek.common.enums.AccountType;
import com.evotek.common.query.PagingQuery;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class EmployeeSearchQuery extends PagingQuery {

    private String keyword;
    private UserStatus status;
    private AccountType accountType;
    private List<String> departmentIds;
}