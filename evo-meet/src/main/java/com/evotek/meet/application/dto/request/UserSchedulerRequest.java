package com.evotek.meet.application.dto.request;

import com.evotek.common.dto.request.Request;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.time.Instant;

@EqualsAndHashCode(callSuper = true)
@Data
public class UserSchedulerRequest extends Request {
    private Instant startAt;
    private Instant finishAt;
}
