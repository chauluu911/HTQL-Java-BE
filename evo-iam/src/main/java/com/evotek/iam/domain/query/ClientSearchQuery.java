package com.evotek.iam.domain.query;

import com.evotek.common.query.PagingQuery;
import com.evotek.iam.infrastructure.support.enums.ClientStatus;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class ClientSearchQuery extends PagingQuery {
    private ClientStatus status;
    private List<String> roleIds;
}
