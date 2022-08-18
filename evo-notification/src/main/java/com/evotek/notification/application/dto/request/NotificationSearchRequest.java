package com.evotek.notification.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.time.Instant;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class NotificationSearchRequest extends PagingRequest {

    @ApiModelProperty(hidden = true)
    private String userId;
    private Instant sendStartAt;
    private Instant sendEndAt;
    private Boolean isRead;
}
