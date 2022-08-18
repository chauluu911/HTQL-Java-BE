package com.evotek.iam.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.iam.domain.Department;
import com.evotek.iam.domain.respository.DepartmentDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.DepartmentEntity;
import com.evotek.iam.infrastructure.persistence.mapper.DepartmentEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.DepartmentEntityRepository;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class DepartmentDomainRepositoryImpl extends AbstractDomainRepository<Department, DepartmentEntity, String> implements DepartmentDomainRepository {

    public DepartmentDomainRepositoryImpl(DepartmentEntityMapper departmentEntityMapper,
                                          DepartmentEntityRepository departmentEntityRepository) {
        super(departmentEntityRepository, departmentEntityMapper);
    }

    @Override
    public Department getById(String id) {
        return this.findById(id).orElseThrow(() -> new ResponseException(BadRequestError.DEPARTMENT_NOT_EXITED));
    }
}
