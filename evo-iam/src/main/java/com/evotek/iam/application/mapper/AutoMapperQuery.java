package com.evotek.iam.application.mapper;

import com.evotek.iam.application.dto.request.*;
import com.evotek.iam.domain.query.*;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface AutoMapperQuery {

    UserSearchQuery toQuery(UserSearchRequest request);

    EmployeeSearchQuery toQuery(EmployeeSearchRequest query);

    DepartmentSearchQuery toQuery(DepartmentSearchRequest query);

    ClientSearchQuery toQuery(ClientSearchRequest request);

    UserGroupSearchQuery toQuery(GroupSearchRequest request );

    JobTitleSearchQuery toQuery(JobTitleSearchRequest request);

    OrganizationSearchQuery toQuery(OrganizationSearchRequest request);
}
