package com.evotek.asset.application.dto.request;

import com.evotek.asset.infrastructure.support.enums.AssetStatus;
import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.Range;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
public class AssetUpdateRequest extends Request {
    @NotBlank(message = "NAME REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "NAME LENGTH")
    private String name;

    @Size(max = ValidateConstraint.LENGTH.SERIAL_NUMBER_MAX_LENGTH, message = "SERIAL NUMBER LENGTH")
    private String serialNumber;

    private AssetStatus status;

    @Size(max = ValidateConstraint.LENGTH.DESC_MAX_LENGTH, message = "DESCRIPTION LENGTH")
    private String description;

    @Size(max = ValidateConstraint.LENGTH.ID_MAX_LENGTH, message = "ASSET TYPE ID LENGTH")
    private String assetTypeId;

    @Size(max = ValidateConstraint.LENGTH.ID_MAX_LENGTH, message = "OWNER ID LENGTH")
    private String ownerId;

    @Range(min = 0)
    private Integer total;

}
