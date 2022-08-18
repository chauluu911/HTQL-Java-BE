package com.evotek.iam.application.service;

import com.evotek.common.dto.PageDTO;
import com.evotek.iam.application.dto.request.RoleAutocompleteRequest;
import com.evotek.iam.application.dto.request.RoleCreateRequest;
import com.evotek.iam.application.dto.request.RoleSearchRequest;
import com.evotek.iam.application.dto.request.RoleUpdateRequest;
import com.evotek.iam.domain.Role;

public interface RoleService {

    PageDTO<Role> search(RoleSearchRequest request);

    Role create(RoleCreateRequest request);

    Role update(String userId, RoleUpdateRequest request);

    Role getById(String id);

    void delete(String id);

    PageDTO<Role> searchByRoleRequest(RoleSearchRequest request);

    PageDTO<Role> autocomplete(RoleAutocompleteRequest request);

    void active(String id);

    void inactive(String id);
}
