package com.evotek.common.queue.message;

import com.evotek.common.queue.AuditableMessage;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.time.Instant;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class ActionLogMessage extends AuditableMessage {

    protected Instant accessedAt;
    private String uri;
    private String method;
    private String userId;
    private String remoteIp;
    private String userAgent;
    private String module;
    private String requestPayload;
    private String requestParam;
    private String responsePayload;
    private Integer httpCode;
    private String hostIp;
    private long duration;

}
