package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class UserGroupCreateRequest extends Request {
    @NotBlank(message = "NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "NAME_LENGTH")
    private String name;

    @Size(max = ValidateConstraint.LENGTH.DESC_MAX_LENGTH, message = "DESC_LENGTH")
    private String description;

    @NotBlank(message = "CODE_REQUIRED")
    @Pattern(regexp = "^[A-Za-z0-9]+$", message = "CODE_FORMAT")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "CODE_LENGTH")
    private String code;

    private List<String> userMemberIds;
}
