package com.evotek.iam.application.service;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.iam.application.dto.request.EmploymentCreateOrUpdateRequest;
import com.evotek.iam.application.dto.request.JobTitleSearchRequest;
import com.evotek.iam.domain.EmploymentHistory;
import com.evotek.iam.domain.JobTitle;
import com.evotek.iam.infrastructure.persistence.mapper.EmployeeEntityMapper;

public interface EmployeeHistoryService {

    EmploymentHistory create(EmploymentCreateOrUpdateRequest request);

}
