package com.evotek.common.dto;

import lombok.Data;

import java.io.Serializable;
import java.time.Instant;

@Data
public class AuditableDTO implements Serializable {

    protected String id;

    protected String createdBy;

    protected Instant createdAt;

    protected String lastModifiedBy;

    protected Instant lastModifiedAt;
}
