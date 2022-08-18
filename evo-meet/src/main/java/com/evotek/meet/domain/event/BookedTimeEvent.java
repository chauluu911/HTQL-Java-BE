package com.evotek.meet.domain.event;

import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.Instant;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class BookedTimeEvent {
    private Instant startAt;
    private Instant finishAt;
}
