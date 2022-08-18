package com.evotek.iam.presentation.web.rest.impl;

import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.iam.application.dto.request.DepartmentAutoCompleteRequest;
import com.evotek.iam.application.dto.request.DepartmentCreateRequest;
import com.evotek.iam.application.dto.request.DepartmentSearchRequest;
import com.evotek.iam.application.dto.request.DepartmentUpdateRequest;
import com.evotek.iam.application.service.DepartmentService;
import com.evotek.iam.domain.Department;
import com.evotek.iam.presentation.web.rest.DepartmentResource;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import java.util.List;

@RestController
public class DepartmentResourceImpl implements DepartmentResource {
    private final DepartmentService departmentService;

    public DepartmentResourceImpl(DepartmentService departmentService) {
        this.departmentService = departmentService;
    }

    @Override
    public Response<List<Department>> getAll() {
        return Response.of(departmentService.getAll());
    }

    @Override
    public Response<List<Department>> getDepartmentByIds(FindByIdsRequest request) {
        return Response.of(departmentService.findByIds(request.getIds()));
    }

    @Override
    public Response<Department> create(@Valid DepartmentCreateRequest request) {
        Department department = departmentService.create(request);
        return Response.of(department);
    }

    @Override
    public Response<Department> update(String id, @Valid DepartmentUpdateRequest request) {
        Department department = departmentService.update(id, request);
        return Response.of(department);
    }

    @Override
    public Response<Department> delete(String id) {
        departmentService.delete(id);
        return Response.ok();
    }

    @Override
    public Response<Department> getTreeView(String id) {
        Department departments = departmentService.getTreeView(id);
        return Response.of(departments);
    }

    @Override
    public PagingResponse<Department> search(DepartmentSearchRequest request) {
        return PagingResponse.of(departmentService.search(request));
    }

    @Override
    public Response<Department> getDepartmentById(String id) {
        return Response.of(departmentService.findById(id));
    }

    @Override
    public Response<Boolean> active(String id) {
        this.departmentService.active(id);
        return Response.ok();
    }

    @Override
    public Response<Boolean> inactive(String id) {
        this.departmentService.inactive(id);
        return Response.ok();

    }

    @Override
    public Response<List<Department>> getAllExcludeProgeny(String id) {
        List<Department> departmentExcept = this.departmentService.getAllDepartmentExcept(id);
        return Response.of(departmentExcept);
    }

    @Override
    public Response<List<Department>> getProgenyByParentIds(List<String> parentIds) {
        List<Department> listChildDepartments = this.departmentService.getChildDepartmentsByParentId(parentIds);
        return Response.of(listChildDepartments);
    }

    @Override
    public PagingResponse<Department> searchAutoComplete(DepartmentAutoCompleteRequest request) {
        return PagingResponse.of(departmentService.searchAutoComplete(request));
    }

    @Override
    public Response<List<Department>> getAllDepartmentRoots() {
        List<Department> listDepartmentRoots = this.departmentService.getAllDepartmentRoot();
        return Response.of(listDepartmentRoots);
    }
}
