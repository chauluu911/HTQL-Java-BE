package com.evotek.meet.domain.command;

import com.evotek.meet.infrastructure.support.enums.RoomStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RoomCreateCmd {

    private String code;
    private String name;
    private String location;
    private RoomStatus status;
}
