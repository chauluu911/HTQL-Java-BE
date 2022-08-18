package com.evotek.iam.application.mapper;

import com.evotek.iam.application.dto.request.*;
import com.evotek.common.ldapcore.dto.request.UserLdapChangePasswordRequest;
import com.evotek.common.ldapcore.dto.request.UserLdapCreateOrUpdateRequest;
import com.evotek.iam.application.dto.request.*;
import com.evotek.iam.application.dto.response.ClientResponse;
import com.evotek.iam.application.dto.response.EmployeeDTO;
import com.evotek.iam.domain.Client;
import com.evotek.iam.domain.Employee;
import com.evotek.iam.domain.command.*;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface AutoMapper {

    UserUpdateCmd from(UserUpdateRequest request);

    UserInternalCreateCmd from(UserInternalCreateRequest request);

    UserCustomerCreateCmd from(UserCustomerCreateRequest request);

    RoleCreateOrUpdateCmd from(RoleCreateRequest request);

    RoleCreateOrUpdateCmd from(RoleUpdateRequest request);

    UserRegisterCmd from(UserRegisterRequest request);

    UserUpdateProfileCmd from(UserUpdateProfileRequest request);

    RolePermissionCreateCmd from(RolePermissionCreateRequest request);

    DepartmentCreateCmd from(DepartmentCreateRequest request);

    DepartmentUpdateCmd from(DepartmentUpdateRequest request);

    EmployeeUpdateCmd from(EmployeeUpdateRequest request);

    JobTitleCreateCmd from(JobTitleCreateRequest request);

    JobTitleUpdateCmd from(JobTitleUpdateRequest request);

    List<EmployeeDTO> toEmployeeDTO(List<Employee> employee);

    EmployeeDTO toEmployeeDTO(Employee employee);

    ClientCreateCmd from(ClientCreateRequest request);

    ClientUpdateCmd from(ClientUpdateRequest request);

    ClientResponse toClientResponse(Client domain);

    UserInternalCreateRequest from(ImportUserCmd importUserCmd);

    UserLdapChangePasswordRequest from(UserChangePasswordRequest request);

    UserLdapCreateOrUpdateRequest fromInternal(UserInternalCreateRequest request);

    UserGroupCreateCmd from(UserGroupCreateRequest request);

    UserGroupUpdateCmd from(UserGroupUpdateRequest request);

    UserGroupMemberUpdateCmd from(UserGroupMemberUpdateRequest request);

    OrganizationCreateCmd from(OrganizationCreateRequest request);

    OrganizationUpdateCmd from(OrganizationUpdateRequest request);
}
