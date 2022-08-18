package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import com.evotek.iam.infrastructure.support.enums.JobTitleStatus;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class JobTitleSearchRequest extends PagingRequest {
    private String keyword;
    private JobTitleStatus status;
}
