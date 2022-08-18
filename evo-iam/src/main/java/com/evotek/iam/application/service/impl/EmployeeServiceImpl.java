package com.evotek.iam.application.service.impl;

import com.evotek.common.client.storage.StorageClient;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.storage.FileDTO;
import com.evotek.common.enums.AccountType;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.StrUtils;
import com.evotek.iam.application.dto.request.EmployeeSearchRequest;
import com.evotek.iam.application.dto.response.EmployeeDTO;
import com.evotek.iam.application.mapper.AutoMapper;
import com.evotek.iam.application.mapper.AutoMapperQuery;
import com.evotek.iam.application.service.EmployeeService;
import com.evotek.iam.domain.*;
import com.evotek.iam.domain.query.EmployeeSearchQuery;
import com.evotek.iam.infrastructure.persistence.entity.DepartmentEntity;
import com.evotek.iam.infrastructure.persistence.entity.EmploymentHistoryEntity;
import com.evotek.iam.infrastructure.persistence.entity.JobTitleEntity;
import com.evotek.iam.infrastructure.persistence.entity.UserEntity;
import com.evotek.iam.infrastructure.persistence.mapper.DepartmentEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.EmployeeEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.EmploymentHistoryEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.JobTitleEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.DepartmentEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.EmploymentHistoryEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.JobTitleEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.UserEntityRepository;
import com.evotek.iam.infrastructure.support.enums.EmploymentStatus;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import com.evotek.iam.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@Slf4j
public class EmployeeServiceImpl implements EmployeeService {

    private final EmployeeEntityMapper employeeEntityMapper;
    private final UserEntityRepository userEntityRepository;
    private final StorageClient storageClient;
    private final AutoMapper autoMapper;
    private final AutoMapperQuery autoMapperQuery;
    private final DepartmentEntityRepository departmentEntityRepository;
    private final EmploymentHistoryEntityRepository employmentHistoryEntityRepository;
    private final DepartmentEntityMapper departmentEntityMapper;
    private final EmploymentHistoryEntityMapper employmentHistoryEntityMapper;
    private final JobTitleEntityMapper jobTitleEntityMapper;
    private final JobTitleEntityRepository jobTitleEntityRepository;

    public EmployeeServiceImpl(EmployeeEntityMapper employeeEntityMapper,
                               UserEntityRepository userEntityRepository,
                               StorageClient storageClient,
                               AutoMapper autoMapper,
                               DepartmentEntityRepository departmentEntityRepository,
                               AutoMapperQuery autoMapperQuery,
                               DepartmentEntityMapper departmentEntityMapper,
                               EmploymentHistoryEntityRepository employmentHistoryEntityRepository,
                               EmploymentHistoryEntityMapper employmentHistoryEntityMapper,
                               JobTitleEntityRepository jobTitleEntityRepository,
                               JobTitleEntityMapper jobTitleEntityMapper
    ) {
        this.employeeEntityMapper = employeeEntityMapper;
        this.userEntityRepository = userEntityRepository;
        this.storageClient = storageClient;
        this.autoMapper = autoMapper;
        this.departmentEntityMapper = departmentEntityMapper;
        this.departmentEntityRepository = departmentEntityRepository;
        this.autoMapperQuery = autoMapperQuery;
        this.employmentHistoryEntityRepository = employmentHistoryEntityRepository;
        this.employmentHistoryEntityMapper = employmentHistoryEntityMapper;
        this.jobTitleEntityRepository = jobTitleEntityRepository;
        this.jobTitleEntityMapper = jobTitleEntityMapper;
    }

    // List employee by filter
    @Override
    public PagingResponse<EmployeeDTO> search(EmployeeSearchRequest request) {
        EmployeeSearchQuery query = autoMapperQuery.toQuery(request);
        if (!CollectionUtils.isEmpty(request.getDepartmentIds())) {
            // Validate
            if (this.departmentEntityRepository
                    .findAllByIds(query.getDepartmentIds()).size() != query.getDepartmentIds().size()) {
                throw new ResponseException(BadRequestError.DEPARTMENT_NOT_EXITED);
            }
            List<Department> departments = this.departmentEntityMapper.toDomain(
                    departmentEntityRepository.getChildDepartmentsByParentIds(query.getDepartmentIds()));
            query.setDepartmentIds(departments.stream().map(Department::getId).collect(Collectors.toList()));
        }
        query.setAccountType(AccountType.EMPLOYEE);
        query.setStatus(UserStatus.ACTIVE);

        List<UserEntity> employeeEntities = this.userEntityRepository.searchEmployees(query);
        List<Employee> employees = this.employeeEntityMapper.toDomain(employeeEntities);
        Long count = this.userEntityRepository.countEmployees(query);
        // get list department
        List<String> departmentIds = employees.stream().map(User::getDepartmentId).distinct().collect(Collectors.toList());
        List<DepartmentEntity> departmentEntities = departmentEntityRepository.findAllById(departmentIds);
        List<Department> departments = departmentEntityMapper.toDomain(departmentEntities);

        employees.forEach(employee -> {
            if (Objects.nonNull(employee.getAvatarFileId())) {
                Response<FileDTO> responseFile = storageClient.findById(employee.getAvatarFileId());
                if (responseFile.isSuccess() && Objects.nonNull(responseFile.getData())) {
                    employee.enrichViewUrlFile(responseFile.getData().getViewUrl());
                }
            }
            // enrich department
            departments.forEach(department -> {
                if (department.getId().equals(employee.getDepartmentId())) {
                    employee.enrichDepartment(department);
                }
            });

            if (Objects.nonNull(employee.getJobTitleId())) {
                if (jobTitleEntityRepository.findById(employee.getJobTitleId()).isPresent()) {
                    employee.enrichJobTitle(jobTitleEntityMapper.toDomain(jobTitleEntityRepository.findById(employee.getJobTitleId()).get()));
                }
            }

        });
        List<EmployeeDTO> result = this.autoMapper.toEmployeeDTO(employees);
        return new PagingResponse<>(result,
                query.getPageIndex(),
                query.getPageSize(),
                count);
    }

    // Find employee by Id
    @Override
    public EmployeeDTO getEmployeeById(String id) {
        Optional<UserEntity> employeeEntity = this.userEntityRepository.findById(id);
        if (employeeEntity.isEmpty()) {
            throw new ResponseException(NotFoundError.EMPLOYEE_NOT_FOUND.getMessage(), NotFoundError.EMPLOYEE_NOT_FOUND, id);
        }
        Employee employee = this.employeeEntityMapper.toDomain(employeeEntity.get());

        if (Objects.nonNull(employee.getAvatarFileId())) {
            Response<FileDTO> responseFile = storageClient.findById(employee.getAvatarFileId());
            if (responseFile.isSuccess() && Objects.nonNull(responseFile.getData())) {
                employee.enrichViewUrlFile(responseFile.getData().getViewUrl());
            }
        }
        // enrich department
        Optional<DepartmentEntity> departmentEntity = this.departmentEntityRepository.findById(employee.getDepartmentId());
        if (departmentEntity.isPresent()) {
            Department department = this.departmentEntityMapper.toDomain(departmentEntity.get());
            employee.enrichDepartmentName(department.getName());
            String departmentRootId = "";
            if (!StrUtils.isBlank(department.getParentPath())) {
                String[] companyId = department.getParentPath().split("/");
                if (companyId.length > 0) {
                    departmentRootId = companyId[0];
                }
            } else {
                departmentRootId = department.getId();
            }
            Optional<DepartmentEntity> departmentEntityRoot = this.departmentEntityRepository.findById(departmentRootId);
            if (departmentEntityRoot.isPresent()) {
                Department departmentRoot = this.departmentEntityMapper.toDomain(departmentEntityRoot.get());
                employee.enrichCompanyCode(departmentRoot.getCode());
            }

        }
        //enrich jobTitle
        Optional<EmploymentHistoryEntity> employmentHistoryEntity = Optional.ofNullable(this.employmentHistoryEntityRepository.findEmploymentHistoryActiveByUserId(employee.getId(), EmploymentStatus.ACTIVE));
        if (employmentHistoryEntity.isPresent()) {
            if (jobTitleEntityRepository.findById(employeeEntity.get().getJobTitleId()).isPresent()) {
                Optional<JobTitleEntity> jobTitleEntityOptional = jobTitleEntityRepository.findById(employeeEntity.get().getJobTitleId());
                jobTitleEntityOptional.ifPresent(jobTitleEntity -> employee.enrichJobTitle(this.jobTitleEntityMapper.toDomain(jobTitleEntity)));
            }
        }
        // enrich EmploymentHistory
        List<EmploymentHistoryEntity> employmentHistoryEntities = this.employmentHistoryEntityRepository.findAllEmploymentHistoriesByUserId(employee.getId());
        if (!employmentHistoryEntities.isEmpty()) {
            List<EmploymentHistory> employmentHistories = this.employmentHistoryEntityMapper.toDomain(employmentHistoryEntities);
            // enrich department for EmploymentHistory
            List<String> departmentIds = employmentHistories.stream().map(EmploymentHistory::getDepartmentId).distinct().collect(Collectors.toList());
            List<DepartmentEntity> departmentEntities = departmentEntityRepository.findAllById(departmentIds);
            List<Department> departments = departmentEntityMapper.toDomain(departmentEntities);
            // enrich department
            employmentHistories.forEach(employmentHistory -> {
                departments.forEach(department -> {
                    if (department.getId().equals(employmentHistory.getDepartmentId())) {
                        employmentHistory.enrichDepartmentName(department.getName());
                    }
                });
            });
            employee.enrichEmploymentHistories(employmentHistories);
        }

        return this.autoMapper.toEmployeeDTO(employee);
    }
}
