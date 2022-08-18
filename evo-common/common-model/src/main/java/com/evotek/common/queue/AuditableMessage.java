package com.evotek.common.queue;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;
import java.time.Instant;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class AuditableMessage implements Serializable {

    protected String sentBy;

    protected Instant sentAt = Instant.now();
}
