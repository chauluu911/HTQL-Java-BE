package com.evotek.iam.presentation.web.rest.impl;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.iam.application.dto.request.JobTitleCreateRequest;
import com.evotek.iam.application.dto.request.JobTitleSearchRequest;
import com.evotek.iam.application.dto.request.JobTitleUpdateRequest;
import com.evotek.iam.application.service.JobTitleService;
import com.evotek.iam.domain.JobTitle;
import com.evotek.iam.presentation.web.rest.JobTitleResource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import java.util.List;

@RestController
public class JobTitleResourceImpl implements JobTitleResource {

    private final JobTitleService jobTitleService;
    public JobTitleResourceImpl(JobTitleService jobTitleService) {
        this.jobTitleService = jobTitleService;
    }

    @Override
    public PagingResponse<JobTitle> search(JobTitleSearchRequest request) {
        return jobTitleService.search(request);
    }

    @Override
    public Response<JobTitle> createJobTitle(@Valid JobTitleCreateRequest request) {
        JobTitle jobTitle = jobTitleService.create(request);
        return Response.of(jobTitle);

    }

    @Override
    public Response<JobTitle> updateJobTitle(String id, @Valid JobTitleUpdateRequest request) {
        JobTitle jobTitle = this.jobTitleService.update(id, request);
        return Response.of(jobTitle);
    }

    @Override
    public Response<Void> deleteJobTitle(String id) {
        this.jobTitleService.delete(id);
        return Response.ok();
    }

    @Override
    public Response<Boolean> activeJobTitle(String id) {
        this.jobTitleService.active(id);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public Response<Boolean> inactiveJobTitle(String id) {
        this.jobTitleService.inactive(id);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public Response<List<JobTitle>> getAllJobTitle(){
        return Response.of(jobTitleService.getAllJobTitle());
    }

    @Override
    public Response<JobTitle> getJobTitleById(String id) {
        return Response.of(jobTitleService.findById(id));
    }

    @Override
    public PagingResponse<JobTitle> autocomplete(JobTitleSearchRequest request) {
        return PagingResponse.of(jobTitleService.autoComplete(request));
    }
}
