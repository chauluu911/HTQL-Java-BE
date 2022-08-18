package com.evotek.iam.presentation.web.rest.impl;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.iam.application.dto.request.EmployeeSearchRequest;
import com.evotek.iam.application.dto.response.EmployeeDTO;
import com.evotek.iam.application.service.EmployeeService;
import com.evotek.iam.presentation.web.rest.EmployeeResource;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class EmployeeResourceImpl implements EmployeeResource {
    private final EmployeeService employeeService;

    public EmployeeResourceImpl(EmployeeService employeeService) {

        this.employeeService = employeeService;
    }

    @Override
    public PagingResponse<EmployeeDTO> search(EmployeeSearchRequest request) {
        return employeeService.search(request);
    }

    @Override
    public Response<EmployeeDTO> getEmployeeById(String id) {
        return Response.of(employeeService.getEmployeeById(id));
    }

}
