package com.evotek.iam.presentation.web.rest;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.iam.application.dto.request.ClientCreateRequest;
import com.evotek.iam.application.dto.request.JobTitleCreateRequest;
import com.evotek.iam.application.dto.request.JobTitleSearchRequest;
import com.evotek.iam.application.dto.request.JobTitleUpdateRequest;
import com.evotek.iam.application.dto.response.ClientResponse;
import com.evotek.iam.domain.JobTitle;
import com.evotek.iam.domain.query.JobTitleSearchQuery;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

@Api(tags = "Job Title Resource")
@RequestMapping("/api")
@Validated
public interface JobTitleResource {

    @ApiOperation(value = "Search Job title")
    @GetMapping("/job-title/search")
    @PreAuthorize("hasPermission(null, 'job-title:view')")
    PagingResponse<JobTitle> search(@ValidatePaging(allowedSorts = {"code", "name", "lastModifiedAt", "createdAt"}) JobTitleSearchRequest request);

    @ApiOperation(value = "Create Job title")
    @PostMapping("/job-title")
    @PreAuthorize("hasPermission(null, 'job-title:create')")
    Response<JobTitle> createJobTitle(@RequestBody @Valid JobTitleCreateRequest request);

    @ApiOperation(value = "Update Job title")
    @PostMapping("/job-title/{id}/update")
    @PreAuthorize("hasPermission(null,'job-title:update')")
    Response<JobTitle> updateJobTitle(@PathVariable("id") String id, @RequestBody @Valid JobTitleUpdateRequest request);

    @ApiOperation(value = "Delete Job title")
    @PostMapping("/job-title/{id}/delete")
    @PreAuthorize("hasPermission(null,'job-title:delete')")
    Response<Void> deleteJobTitle(@PathVariable String id);

    @ApiOperation(value = "Active Job title")
    @PostMapping("/job-title/{id}/active")
    @PreAuthorize("hasPermission(null,'job-title:update')")
    Response<Boolean> activeJobTitle(@PathVariable String id);

    @ApiOperation(value = "Inactive Job title")
    @PostMapping("/job-title/{id}/inactive")
    @PreAuthorize("hasPermission(null,'job-title:update')")
    Response<Boolean> inactiveJobTitle(@PathVariable String id);

    @ApiOperation(value = "View all job title")
    @GetMapping("/job-title")
    @PreAuthorize("hasPermission(null,'job-title:view')")
    Response<List<JobTitle>> getAllJobTitle();

    @ApiOperation(value = "View job title by id")
    @GetMapping("/job-title/{id}")
    @PreAuthorize("hasPermission(null,'job-title:view')")
    Response<JobTitle> getJobTitleById(@PathVariable String id);

    @ApiOperation(value = "Search Job title auto complete")
    @GetMapping("/job-title/auto-complete")
    @PreAuthorize("hasPermission(null,'job-title:view')")
    PagingResponse<JobTitle> autocomplete(@ValidatePaging(allowedSorts = {"code, name,lastModifiedAt, createdAt"}) JobTitleSearchRequest request);

}