package com.evotek.iam.presentation.web.rest;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.iam.application.dto.request.EmployeeSearchRequest;
import com.evotek.iam.application.dto.response.EmployeeDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

@Api(tags = "Employee Resource")
@RequestMapping("/api")
@Validated
public interface EmployeeResource {
    @ApiOperation(value = "Get all employee")
    @GetMapping("/employees")
    @PreAuthorize("hasPermission(null,'employee:view')")
    PagingResponse<EmployeeDTO> search(@ValidatePaging(allowedSorts = {"fullName", "email"})
                                               EmployeeSearchRequest request);

    @ApiOperation(value = "Get employee by Id")
    @GetMapping("/employees/{id}")
    @PreAuthorize("hasPermission(null,'employee:view')")
    Response<EmployeeDTO> getEmployeeById(@PathVariable String id);

}
