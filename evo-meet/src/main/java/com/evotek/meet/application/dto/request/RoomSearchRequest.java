package com.evotek.meet.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import com.evotek.meet.infrastructure.support.enums.RoomStatus;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
public class RoomSearchRequest extends PagingRequest {

    private String name;
    private String location;
    private String code;
    private String keyword;
    private RoomStatus status;
}
