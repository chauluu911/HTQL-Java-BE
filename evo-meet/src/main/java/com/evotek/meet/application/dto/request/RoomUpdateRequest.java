package com.evotek.meet.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.meet.infrastructure.support.enums.RoomStatus;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
public class RoomUpdateRequest extends Request {

    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "NAME_LENGTH")
    private String name;

    @Size(max = ValidateConstraint.LENGTH.ADDRESS_MAX_LENGTH, message = "LOCATION_LENGTH")
    private String location;

    private RoomStatus status;
}
