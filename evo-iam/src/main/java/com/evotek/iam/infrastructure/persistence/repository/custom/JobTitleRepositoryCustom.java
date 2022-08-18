package com.evotek.iam.infrastructure.persistence.repository.custom;

import com.evotek.iam.application.dto.request.JobTitleSearchRequest;
import com.evotek.iam.domain.JobTitle;
import com.evotek.iam.domain.query.JobTitleSearchQuery;
import com.evotek.iam.infrastructure.persistence.entity.JobTitleEntity;

import java.util.List;

public interface JobTitleRepositoryCustom {
    List<JobTitleEntity> search(JobTitleSearchQuery query);

    Long count(JobTitleSearchQuery query);
}
