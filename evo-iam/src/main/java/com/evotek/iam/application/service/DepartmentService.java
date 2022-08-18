package com.evotek.iam.application.service;

import com.evotek.common.dto.PageDTO;
import com.evotek.iam.application.dto.request.DepartmentAutoCompleteRequest;
import com.evotek.iam.application.dto.request.DepartmentCreateRequest;
import com.evotek.iam.application.dto.request.DepartmentSearchRequest;
import com.evotek.iam.application.dto.request.DepartmentUpdateRequest;
import com.evotek.iam.domain.Department;

import java.util.List;

public interface DepartmentService {

    List<Department> getAll();

    List<Department> findByIds(List<String> ids);

    Department create(DepartmentCreateRequest request);

    Department update(String id, DepartmentUpdateRequest request);

    void delete(String id);

    List<Department> findAllByParentId(String uuid);

    Department getTreeView(String id);

    PageDTO<Department> search(DepartmentSearchRequest request);

    void active(String id);

    void inactive(String id);

    List<Department> getAllDepartmentExcept(String id);

    List<Department> getChildDepartmentsByParentId(List<String> parentIds);

    PageDTO<Department> searchAutoComplete(DepartmentAutoCompleteRequest request);

    Department findById(String id);

    List<Department> getAllDepartmentRoot();
}
