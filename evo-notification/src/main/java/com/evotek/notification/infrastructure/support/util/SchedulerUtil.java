package com.evotek.notification.infrastructure.support.util;

import com.github.kagkarlsson.scheduler.Scheduler;

import static org.slf4j.LoggerFactory.getLogger;

public class SchedulerUtil {
    private SchedulerUtil() {
        throw new IllegalStateException("Utility class");
    }
    public static void registerShutdownHook(Scheduler scheduler) {
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            getLogger(SchedulerUtil.class).info("Received shutdown signal.");
            scheduler.stop();
        }));

    }
}
