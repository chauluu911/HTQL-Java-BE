package com.evotek.iam.application.service.impl;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.exception.ResponseException;
import com.evotek.iam.application.dto.request.JobTitleCreateRequest;
import com.evotek.iam.application.dto.request.JobTitleSearchRequest;
import com.evotek.iam.application.dto.request.JobTitleUpdateRequest;
import com.evotek.iam.application.mapper.AutoMapper;
import com.evotek.iam.application.mapper.AutoMapperQuery;
import com.evotek.iam.application.service.JobTitleService;
import com.evotek.iam.domain.JobTitle;
import com.evotek.iam.domain.command.JobTitleCreateCmd;
import com.evotek.iam.domain.command.JobTitleUpdateCmd;
import com.evotek.iam.domain.command.UserUpdateCmd;
import com.evotek.iam.domain.query.JobTitleSearchQuery;
import com.evotek.iam.domain.respository.JobTitleDomainRepository;
import com.evotek.iam.domain.respository.UserDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.EmploymentHistoryEntity;
import com.evotek.iam.infrastructure.persistence.entity.JobTitleEntity;
import com.evotek.iam.infrastructure.persistence.entity.UserEntity;
import com.evotek.iam.infrastructure.persistence.mapper.JobTitleEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.PermissionEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.RoleEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.EmploymentHistoryEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.JobTitleEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.PermissionEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.UserEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.custom.JobTitleRepositoryCustom;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import lombok.NoArgsConstructor;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

@Service
@Slf4j
@RequiredArgsConstructor
public class JobTitleServiceImpl implements JobTitleService {

    private final JobTitleDomainRepository jobTitleDomainRepository;
    private final JobTitleEntityRepository jobTitleEntityRepository;
    private final PermissionEntityRepository permissionEntityRepository;
    private final JobTitleEntityMapper jobTitleEntityMapper;
    private final PermissionEntityMapper permissionEntityMapper;
    private final EmploymentHistoryEntityRepository employmentHistoryEntityRepository;
    private final UserEntityRepository userEntityRepository;
    private final AutoMapper autoMaper;
    private final AutoMapperQuery autoMapperQuery;
    private final JobTitleRepositoryCustom jobTitleRepositoryCustom;

    @Override
    @Transactional
    public JobTitle create(JobTitleCreateRequest request) {
        Optional<JobTitleEntity> jobTitleEntity = jobTitleEntityRepository.findByJobTitleCode(request.getCode());
        if (jobTitleEntity.isPresent()) {
            throw new ResponseException(BadRequestError.JOB_TITLE_CODE_EXISTED);
        }
        JobTitleCreateCmd cmd = autoMaper.from(request);
        JobTitle jobTitle = new JobTitle(cmd);
        jobTitleDomainRepository.save(jobTitle);

        return jobTitle;
    }

    @Override
    @Transactional
    public JobTitle update(String id, JobTitleUpdateRequest request) {
        JobTitle jobTitle = this.jobTitleDomainRepository.getById(id);
        JobTitleUpdateCmd cmd = autoMaper.from(request);
        jobTitle.update(cmd);
        this.jobTitleDomainRepository.save(jobTitle);

        return jobTitle;
    }

    @Override
    @Transactional
    public void delete(String id) {
        Optional<JobTitleEntity> jobTitleEntity = this.jobTitleEntityRepository.findById(id);
        if (jobTitleEntity.isEmpty()) {
            throw new ResponseException(BadRequestError.JOB_TITLE_DOES_NOT_EXISTED);
        }
        Optional<EmploymentHistoryEntity> employmentHistoryEntity = this.employmentHistoryEntityRepository.findEmploymentHistoryByJobTitleId(id);
        if (employmentHistoryEntity.isPresent()) {
            throw new ResponseException(BadRequestError.JOB_TITLE_ALREADY_IN_USED);
        }
        Optional<UserEntity> userEntity = this.userEntityRepository.findFirstUserByJobTitleId(id);
        if (userEntity.isPresent()) {
            throw new ResponseException(BadRequestError.JOB_TITLE_ALREADY_IN_USED);
        }
        JobTitle jobTitle = jobTitleEntityMapper.toDomain(jobTitleEntity.get());
        jobTitle.delete();
        this.jobTitleDomainRepository.save(jobTitle);

    }

    @Override
    public JobTitle findByJobTitleCode(String jobTitleCode) {
        Optional<JobTitleEntity> jobTitleEntity = this.jobTitleEntityRepository.findByJobTitleCode(jobTitleCode);
        if (jobTitleEntity.isPresent()) {
            return jobTitleEntityMapper.toDomain(jobTitleEntity.get());
        } else {
            throw new ResponseException(BadRequestError.JOB_TITLE_DOES_NOT_EXISTED);
        }
    }

    @Override
    public JobTitle findById(String id) {
        return this.jobTitleDomainRepository.getById(id);
    }

    @Override
    @Transactional
    public void active(String id) {
        JobTitle jobTitle = this.jobTitleDomainRepository.getById(id);
        jobTitle.active();
        this.jobTitleDomainRepository.save(jobTitle);
    }

    @Override
    @Transactional
    public void inactive(String id) {
        JobTitle jobTitle = this.jobTitleDomainRepository.getById(id);
        jobTitle.inactive();
        this.jobTitleDomainRepository.save(jobTitle);
    }

    @Override
    public List<JobTitle> getAllJobTitle() {
        List<JobTitleEntity> lists = this.jobTitleEntityRepository.findAllJobTitle();
        return jobTitleEntityMapper.toDomain(lists);
    }

    @Override
    public PagingResponse<JobTitle> search(JobTitleSearchRequest request) {
        JobTitleSearchQuery query = autoMapperQuery.toQuery(request);
        List<JobTitleEntity> jobTitleEntities = jobTitleRepositoryCustom.search(query);
        List<JobTitle> jobTitles = jobTitleEntityMapper.toDomain(jobTitleEntities);
        return new PagingResponse<>(jobTitles, request.getPageIndex(), request.getPageSize(), jobTitleRepositoryCustom.count(query));
    }

    @Override
    public PageDTO<JobTitle> autoComplete(JobTitleSearchRequest request) {
        JobTitleSearchQuery query = autoMapperQuery.toQuery(request);
        List<JobTitleEntity> jobTitleEntities = jobTitleRepositoryCustom.search(query);

        return PageDTO.of(jobTitleEntityMapper.toDomain(jobTitleEntities),
                request.getPageIndex(),
                request.getPageSize(),
                jobTitleRepositoryCustom.count(query));
    }

}
