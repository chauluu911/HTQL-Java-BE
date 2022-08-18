package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.time.Instant;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class RoleSearchRequest extends PagingRequest {

    private String isRoot;

    private String status;

    private Instant startAt;

    private List<String> createdBy;

    private Instant endAt;

    private String keyword;

    private Instant startCreatedAt;

    private Instant endCreatedAt;

    private Instant startLastModifiedAt;

    private Instant endLastModifiedAt;
}
