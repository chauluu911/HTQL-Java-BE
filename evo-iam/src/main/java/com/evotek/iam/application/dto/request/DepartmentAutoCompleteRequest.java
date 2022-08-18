package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import com.evotek.iam.infrastructure.support.enums.DepartmentStatus;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;


@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class DepartmentAutoCompleteRequest extends PagingRequest {
    private String keyword;
    private DepartmentStatus status;
    private List<String> ids;
}
