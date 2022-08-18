package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class EmployeeSearchRequest extends PagingRequest {
    private String keyword;
    private List<String> departmentIds;
}
