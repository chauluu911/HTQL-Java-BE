package com.evotek.asset.application.dto.request;

import com.evotek.asset.infrastructure.support.enums.AssetTypeStatus;
import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
public class AssetTypeCreateRequest extends Request {

    @NotBlank(message = "CODE_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "CODE_LENGTH")
    private String code;

    @NotBlank(message = "NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "NAME_LENGTH")
    private String name;

    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "DESCRIPTION_LENGTH")
    private String description;

    private AssetTypeStatus status;
}
