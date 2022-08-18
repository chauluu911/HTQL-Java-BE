package com.evotek.iam.domain.query;

import com.evotek.common.query.PagingQuery;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class UserSearchQuery extends PagingQuery {
    private String status;
    private String accountType;
    private List<String> roleIds;
    private List<String> userIds;
    private boolean searchByGroup;
}
