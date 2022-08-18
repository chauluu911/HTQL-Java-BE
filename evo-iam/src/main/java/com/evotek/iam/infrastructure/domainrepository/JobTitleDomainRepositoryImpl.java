package com.evotek.iam.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.mapper.EntityMapper;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.iam.domain.JobTitle;
import com.evotek.iam.domain.respository.JobTitleDomainRepository;
import com.evotek.iam.domain.respository.UserDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.JobTitleEntity;
import com.evotek.iam.infrastructure.persistence.mapper.JobTitleEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.UserEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.UserRoleEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.JobTitleEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.UserEntityRepository;
import com.evotek.iam.infrastructure.persistence.repository.UserRoleEntityRepository;
import com.evotek.iam.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Slf4j
public class JobTitleDomainRepositoryImpl extends AbstractDomainRepository<JobTitle, JobTitleEntity, String> implements JobTitleDomainRepository {

    private final JobTitleEntityRepository jobTitleEntityRepository;
    private final JobTitleEntityMapper jobTitleEntityMapper;

    protected JobTitleDomainRepositoryImpl(JobTitleEntityRepository jobTitleEntityRepository,
                                           JobTitleEntityMapper jobTitleEntityMapper) {
        super(jobTitleEntityRepository, jobTitleEntityMapper);
        this.jobTitleEntityRepository = jobTitleEntityRepository;
        this.jobTitleEntityMapper = jobTitleEntityMapper;
    }

    @Override
    @Transactional
    public JobTitle save(JobTitle domain) {
        JobTitleEntity jobTitleEntity = jobTitleEntityMapper.toEntity(domain);
        this.jobTitleEntityRepository.save(jobTitleEntity);
        return domain;
    }

    @Override
    public JobTitle getById(String id) {
        return this.findById(id).orElseThrow(()->{
            throw new ResponseException(NotFoundError.JOB_TITLE_NOT_FOUND);
        });
    }

    @Override
    public List saveAll(List domains) {
        return this.saveAll(jobTitleEntityMapper.toEntity(domains));
    }

}
