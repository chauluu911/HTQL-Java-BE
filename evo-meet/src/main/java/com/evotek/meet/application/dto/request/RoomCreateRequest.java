package com.evotek.meet.application.dto.request;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.meet.infrastructure.support.enums.RoomStatus;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
public class RoomCreateRequest extends Request {

    @NotBlank(message = "ROOM_CODE_REQUIRED")
    @Pattern(regexp = "^[A-Za-z0-9]+$", message = "FORMAT_ROOM_CODE")
    @Size(max = ValidateConstraint.LENGTH.CODE_MAX_LENGTH, message = "CODE_LENGTH")
    private String code;

    @NotBlank(message = "ROOM_NAME_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "NAME_LENGTH")
    private String name;

    @NotBlank(message = "ROOM_LOCATION_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.ADDRESS_MAX_LENGTH, message = "LOCATION_LENGTH")
    private String location;

    private RoomStatus status;
}
