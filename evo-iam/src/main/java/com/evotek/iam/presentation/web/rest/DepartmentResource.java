package com.evotek.iam.presentation.web.rest;

import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.iam.application.dto.request.DepartmentAutoCompleteRequest;
import com.evotek.iam.application.dto.request.DepartmentCreateRequest;
import com.evotek.iam.application.dto.request.DepartmentSearchRequest;
import com.evotek.iam.application.dto.request.DepartmentUpdateRequest;
import com.evotek.iam.domain.Department;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

@Api(tags = "Department Resource")
@Validated
@RequestMapping(value = "/api")
public interface DepartmentResource {

    @GetMapping("/departments")
    @ApiOperation(value = "Get All Department")
    @PreAuthorize("hasPermission(null,'department:view')")
    Response<List<Department>> getAll();

    @PostMapping("/departments/find-by-ids")
    @ApiOperation(value = "Get Department By Ids")
    @PreAuthorize("hasPermission(null,'department:view')")
    Response<List<Department>> getDepartmentByIds(@RequestBody FindByIdsRequest request);

    @PostMapping("/departments")
    @ApiOperation(value = "Create Department")
    @PreAuthorize("hasPermission(null,'department:view')")
    Response<Department> create(@Valid @RequestBody DepartmentCreateRequest request);

    @PostMapping("/departments/{id}/update")
    @ApiOperation(value = "Update Department")
    @PreAuthorize("hasPermission(null,'department:update')")
    Response<Department> update(@PathVariable String id, @Valid @RequestBody DepartmentUpdateRequest request);

    @PostMapping("/departments/{id}/delete")
    @ApiOperation(value = "Delete Department")
    @PreAuthorize("hasPermission(null,'department:delete')")
    Response<Department> delete(@PathVariable String id);

    @GetMapping("/departments/{id}/tree-view")
    @ApiOperation(value = "Get Department Tree")
    @PreAuthorize("hasPermission(null,'department:view')")
    Response<Department> getTreeView(@PathVariable String id);

    @GetMapping("/departments/search")
    @ApiOperation(value = "Search Department")
    @PreAuthorize("hasPermission(null,'department:view')")
    PagingResponse<Department> search(@ValidatePaging(allowedSorts = {"createdAt", "lastModifiedAt", "name", "code"})
                                              DepartmentSearchRequest request);

    @GetMapping("/departments/{id}")
    @ApiOperation(value = "Find Department By Id")
    @PreAuthorize("hasPermission(null,'department:view')")
    Response<Department> getDepartmentById(@PathVariable String id);

    @ApiOperation(value = "Active Department")
    @PostMapping("/departments/{id}/active")
    @PreAuthorize("hasPermission(null,'department:update')")
    Response<Boolean> active(@PathVariable String id);

    @ApiOperation(value = "Inactive Department")
    @PostMapping("/departments/{id}/inactive")
    @PreAuthorize("hasPermission(null,'department:update')")
    Response<Boolean> inactive(@PathVariable String id);

    @ApiOperation(value = "Get All Department Exclude Progeny")
    @GetMapping("departments/{id}/exclude-progeny")
    @PreAuthorize("hasPermission(null,'department:view')")
    Response<List<Department>> getAllExcludeProgeny(@PathVariable String id);

    @ApiOperation(value = "Get All Progeny Department Id")
    @GetMapping("departments/find-all-progeny-by-parent-ids")
    @PreAuthorize("hasPermission(null,'department:view')")
    Response<List<Department>> getProgenyByParentIds(@RequestParam(value = "parentIds") List<String> parentIds);

    @ApiOperation(value = "Get All Progeny Department Id")
    @GetMapping("departments/auto-complete")
    @PreAuthorize("hasPermission(null,'department:view')")
    PagingResponse<Department> searchAutoComplete(DepartmentAutoCompleteRequest request);

    @ApiOperation(value = "Get All Department Roots")
    @GetMapping("departments/roots")
    @PreAuthorize("hasPermission(null,'department:view')")
    Response<List<Department>> getAllDepartmentRoots();
}
