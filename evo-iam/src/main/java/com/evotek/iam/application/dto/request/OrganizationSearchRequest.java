package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.time.LocalDate;

@EqualsAndHashCode(callSuper = true)
@Data
public class OrganizationSearchRequest extends PagingRequest {

    private String keyword;

    private String status;

    private LocalDate incorporationDate;
}

