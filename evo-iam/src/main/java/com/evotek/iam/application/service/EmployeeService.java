package com.evotek.iam.application.service;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.iam.application.dto.request.EmployeeSearchRequest;
import com.evotek.iam.application.dto.response.EmployeeDTO;

public interface EmployeeService {

    PagingResponse<EmployeeDTO> search(EmployeeSearchRequest request);

    EmployeeDTO getEmployeeById(String id);
}
