package com.evotek.iam.application.service.impl;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.mapper.util.PageableMapperUtil;
import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.iam.application.dto.request.DepartmentAutoCompleteRequest;
import com.evotek.iam.application.dto.request.DepartmentCreateRequest;
import com.evotek.iam.application.dto.request.DepartmentSearchRequest;
import com.evotek.iam.application.dto.request.DepartmentUpdateRequest;
import com.evotek.iam.application.mapper.AutoMapper;
import com.evotek.iam.application.mapper.AutoMapperQuery;
import com.evotek.iam.application.service.DepartmentService;
import com.evotek.iam.domain.Department;
import com.evotek.iam.domain.EmploymentHistory;
import com.evotek.iam.domain.command.DepartmentCreateCmd;
import com.evotek.iam.domain.command.DepartmentUpdateCmd;
import com.evotek.iam.domain.query.DepartmentSearchQuery;
import com.evotek.iam.domain.respository.DepartmentDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.DepartmentEntity;
import com.evotek.iam.infrastructure.persistence.entity.EmploymentHistoryEntity;
import com.evotek.iam.infrastructure.persistence.entity.JobTitleEntity;
import com.evotek.iam.infrastructure.persistence.entity.UserEntity;
import com.evotek.iam.infrastructure.persistence.mapper.DepartmentEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.EmploymentHistoryEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.JobTitleEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.UserEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.DepartmentEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.EmploymentHistoryEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.JobTitleEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.UserEntityRepository;
import com.evotek.iam.infrastructure.support.enums.DepartmentStatus;
import com.evotek.iam.infrastructure.support.enums.EmploymentStatus;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import com.evotek.iam.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@Slf4j
public class DepartmentServiceImpl implements DepartmentService {

    private final DepartmentDomainRepository departmentDomainRepository;
    private final DepartmentEntityMapper departmentEntityMapper;
    private final DepartmentEntityRepository departmentEntityRepository;
    private final EmploymentHistoryEntityRepository employmentHistoryEntityRepository;
    private final UserEntityRepository userEntityRepository;
    private final EmploymentHistoryEntityMapper employmentHistoryEntityMapper;
    private final UserEntityMapper userEntityMapper;
    private final JobTitleEntityRepository jobTitleEntityRepository;
    private final JobTitleEntityMapper jobTitleEntityMapper;


    private final AutoMapper autoMapper;
    private final AutoMapperQuery autoMapperQuery;

    public DepartmentServiceImpl(DepartmentDomainRepository departmentDomainRepository,
                                 DepartmentEntityMapper departmentEntityMapper,
                                 DepartmentEntityRepository departmentEntityRepository,
                                 EmploymentHistoryEntityRepository employmentHistoryEntityRepository,
                                 EmploymentHistoryEntityMapper employmentHistoryEntityMapper,
                                 UserEntityRepository userEntityRepository,
                                 UserEntityMapper userEntityMapper,
                                 AutoMapper autoMapper, AutoMapperQuery autoMapperQuery,
                                 JobTitleEntityRepository jobTitleEntityRepository,
                                 JobTitleEntityMapper jobTitleEntityMapper) {
        this.departmentDomainRepository = departmentDomainRepository;
        this.departmentEntityMapper = departmentEntityMapper;
        this.departmentEntityRepository = departmentEntityRepository;
        this.autoMapper = autoMapper;
        this.userEntityMapper = userEntityMapper;
        this.userEntityRepository = userEntityRepository;
        this.employmentHistoryEntityMapper = employmentHistoryEntityMapper;
        this.employmentHistoryEntityRepository = employmentHistoryEntityRepository;
        this.autoMapperQuery = autoMapperQuery;
        this.jobTitleEntityRepository = jobTitleEntityRepository;
        this.jobTitleEntityMapper = jobTitleEntityMapper;
    }

    @Override
    public List<Department> getAll() {
        List<DepartmentEntity> list = departmentEntityRepository.findAllActivated();
        return departmentEntityMapper.toDomain(list);
    }

    @Override
    public List<Department> findByIds(List<String> ids) {
        List<DepartmentEntity> departmentEntities = departmentEntityRepository.findAllByIds(ids);
        return departmentEntityMapper.toDomain(departmentEntities);
    }

    @Transactional
    @Override
    public Department create(DepartmentCreateRequest request) {
        Optional<DepartmentEntity> departmentEntity = departmentEntityRepository.findByCode(request.getCode());
        if (departmentEntity.isPresent()) {
            throw new ResponseException(BadRequestError.DEPARTMENT_CODE_EXISTED);
        }
        DepartmentCreateCmd cmd = autoMapper.from(request);
        if (!StringUtils.hasLength(request.getParentId())) {
            Department department = new Department(cmd, null);
            this.departmentDomainRepository.save(department);
            return department;
        }
        departmentEntity = departmentEntityRepository.findByDepartmentId(request.getParentId());
        if (departmentEntity.isEmpty()) {
            throw new ResponseException(BadRequestError.DEPARTMENT_NOT_EXITED);
        }
        Department parent = departmentEntityMapper.toDomain(departmentEntity.get());
        Department department = new Department(cmd, parent);
        this.departmentDomainRepository.save(department);
        return department;
    }

    @Transactional
    @Override
    public Department update(String id, DepartmentUpdateRequest request) {
        Optional<DepartmentEntity> departmentEntity = departmentEntityRepository.findById(id);
        if (departmentEntity.isEmpty()) {
            throw new ResponseException(BadRequestError.DEPARTMENT_NOT_EXITED);
        }
        DepartmentUpdateCmd cmd = autoMapper.from(request);
        if (!StringUtils.hasLength(request.getParentId())) {
            Department department = departmentEntityMapper.toDomain(departmentEntity.get());
            department.update(cmd, null);
            this.departmentDomainRepository.save(department);
            return department;
        }
        Department department = departmentEntityMapper.toDomain(departmentEntity.get());
        Optional<DepartmentEntity> newParentEntity = departmentEntityRepository.findByDepartmentId(request.getParentId());
        if (newParentEntity.isEmpty()) {
            throw new ResponseException(BadRequestError.DEPARTMENT_NOT_EXITED);
        }
        Department newParent = departmentEntityMapper.toDomain(newParentEntity.get());
        List<DepartmentEntity> childrenDepartmentEntities = departmentEntityRepository.getChildDepartmentsByParentIds(List.of(department.getId()));
        List<Department> childrenDepartments = departmentEntityMapper
                .toDomain(childrenDepartmentEntities.stream().filter(it -> !Objects.equals(it.getId(), id)).collect(Collectors.toList()));
        department.enrichChildren(childrenDepartments);
        department.update(cmd, newParent);
        List<Department> departments = new ArrayList<>();
        departments.add(department);
        departments.addAll(childrenDepartments);
        this.departmentDomainRepository.saveAll(departments);
        return department;
    }

    @Transactional
    @Override
    public void delete(String id) {
        Department department = this.departmentDomainRepository.getById(id);
        department.delete();
        this.departmentDomainRepository.save(department);
    }

    @Override
    public List<Department> findAllByParentId(String parentId) {
        this.departmentDomainRepository.getById(parentId);
        List<DepartmentEntity> departmentEntities = departmentEntityRepository.findAllByParentActivated(parentId);
        List<Department> departments = departmentEntityMapper.toDomain(departmentEntities);
        log.info("Find department by parent {}, count {}", parentId, departments.size());
        return departments;
    }

    @Override
    public Department getTreeView(String id) {
        Department department = this.departmentDomainRepository.getById(id);
        List<Department> departments = departmentEntityMapper
                .toDomain(this.departmentEntityRepository.getChildDepartmentsByParentIds(List.of(id)));
        return this.enrichProgeny(department, departments);
    }

    @Override
    public PageDTO<Department> search(DepartmentSearchRequest request) {
        DepartmentSearchQuery query = this.autoMapperQuery.toQuery(request);
        Long count = this.departmentEntityRepository.count(query);
        if (Objects.equals(count, 0L)) {
            return PageDTO.empty();
        }
        List<Department> departments = this.departmentEntityMapper
                .toDomain(this.departmentEntityRepository.search(query));
        log.info("Find department by search request {}, count {}", request, departments.size());
        return PageDTO.of(departments, request.getPageIndex(), request.getPageSize(), count);
    }

    @Override
    @Transactional
    public void active(String id) {
        Department department = this.departmentDomainRepository.getById(id);
        department.active();
        this.departmentDomainRepository.save(department);
    }

    @Override
    @Transactional
    public void inactive(String id) {
        Department department = this.departmentDomainRepository.getById(id);
        department.inactive();
        this.departmentDomainRepository.save(department);
    }

    @Override
    public List<Department> getAllDepartmentExcept(String id) {
        this.departmentDomainRepository.getById(id);
        List<DepartmentEntity> listDepartment = departmentEntityRepository.findAllExceptActivated(id);
        return departmentEntityMapper.toDomain(listDepartment);
    }

    @Override
    public List<Department> getChildDepartmentsByParentId(List<String> parentIds) {
        if (CollectionUtils.isEmpty(parentIds)) {
            throw new ResponseException(BadRequestError.DEPARTMENT_IDS_REQUIRED);
        }
        List<DepartmentEntity> children = departmentEntityRepository.getChildDepartmentsByParentIds(parentIds);
        return departmentEntityMapper.toDomain(children);
    }

    @Override
    public PageDTO<Department> searchAutoComplete(DepartmentAutoCompleteRequest request) {
        Pageable pageable = PageableMapperUtil.toPageable(request);
        DepartmentStatus status = request.getStatus();
        Page<DepartmentEntity> pageDepartment = this.departmentEntityRepository.searchAutoComplete(
                SqlUtils.encodeKeyword(request.getKeyword()), status, request.getIds(), pageable);
        return new PageDTO<>(departmentEntityMapper.toDomain(pageDepartment.getContent()),
                request.getPageIndex(), request.getPageSize(), pageDepartment.getTotalElements());
    }

    @Override
    public List<Department> getAllDepartmentRoot() {
        List<DepartmentEntity> listDepartmentRootEntities = this.departmentEntityRepository.findAllRoots();
        return this.departmentEntityMapper.toDomain(listDepartmentRootEntities);
    }

    @Override
    public Department findById(String id) {
        Optional<DepartmentEntity> departmentEntity = this.departmentEntityRepository.findById(id);
        if (departmentEntity.isEmpty()) {
            throw new ResponseException(NotFoundError.DEPARTMENT_NOT_FOUND.getMessage(), NotFoundError.DEPARTMENT_NOT_FOUND, id);
        }
        Department department = this.departmentEntityMapper.toDomain(departmentEntity.get());
        // get list employee manager of department
        List<JobTitleEntity> jobTitleEntities = jobTitleEntityRepository.findJobTitleIsManager(Boolean.TRUE);
        List<String> jobTitleIds = jobTitleEntities.stream().map(JobTitleEntity::getId).collect(Collectors.toList());

        List<EmploymentHistoryEntity> employmentHistoryEntities = this.employmentHistoryEntityRepository.findEmploymentHistoryActiveManager(department.getId(), jobTitleIds, EmploymentStatus.ACTIVE);
        if (!employmentHistoryEntities.isEmpty()) {
            List<EmploymentHistory> employmentHistories = this.employmentHistoryEntityMapper.toDomain(employmentHistoryEntities);
            List<UserEntity> userEntities = this.userEntityRepository.findByIds(
                    employmentHistoryEntities.stream().map(EmploymentHistoryEntity::getUserId).collect(Collectors.toList()));
            department.enrichEmploymentHistory(employmentHistories, this.userEntityMapper.toDomain(userEntities));
        }
        return department;
    }

    private Department enrichProgeny(Department department, List<Department> departments) {
        List<Department> departmentsOfParent =
                departments.stream().filter(d -> Objects.equals(department.getId(), d.getParentId())).collect(Collectors.toList());
        department.enrichChildren(departmentsOfParent);
        departmentsOfParent.forEach(d -> this.enrichProgeny(d, departments));
        return department;
    }
}
