package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.enums.Scope;
import com.evotek.common.validator.ValidateConstraint;
import lombok.*;

import javax.validation.constraints.Size;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RolePermissionCreateRequest extends Request {

    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "ROLE_PERMISSION_RESOURCE_LENGTH")
    private String resourceCode;

    private List<Scope> scopes;
}
