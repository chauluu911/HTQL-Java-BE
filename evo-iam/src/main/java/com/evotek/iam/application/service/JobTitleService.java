package com.evotek.iam.application.service;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.iam.application.dto.request.JobTitleCreateRequest;
import com.evotek.iam.application.dto.request.JobTitleSearchRequest;
import com.evotek.iam.application.dto.request.JobTitleUpdateRequest;
import com.evotek.iam.domain.JobTitle;

import java.util.List;

public interface JobTitleService {

    JobTitle create(JobTitleCreateRequest request);

    JobTitle update(String id, JobTitleUpdateRequest request);

    void delete(String id);

    JobTitle findByJobTitleCode(String jobTitleCode);

    JobTitle findById(String id);

    void active(String id);

    void inactive(String id);

    List<JobTitle> getAllJobTitle();

    PagingResponse<JobTitle> search(JobTitleSearchRequest request);

    PageDTO<JobTitle> autoComplete(JobTitleSearchRequest request);
}
