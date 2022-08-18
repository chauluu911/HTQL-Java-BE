package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class UserGroupUpdateRequest extends Request {
    @NotBlank(message = "NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "NAME_LENGTH")
    private String name;

    @Size(max = ValidateConstraint.LENGTH.DESC_MAX_LENGTH, message = "DESC_LENGTH")
    private String description;

    private List<String> userMemberIds;
}
