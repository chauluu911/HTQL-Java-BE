package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.Instant;


@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class DepartmentSearchRequest extends PagingRequest {
    private String keyword;

    private Instant startAt;

    private Instant endAt;

    private String status;

    private Instant startCreatedAt;

    private Instant endCreatedAt;

    private Instant startLastModifiedAt;

    private Instant endLastModifiedAt;
}
