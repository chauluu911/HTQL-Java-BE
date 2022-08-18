package com.evotek.iam.application.config;

import com.evotek.iam.application.service.SyncService;
import com.github.kagkarlsson.scheduler.SchedulerName;
import com.github.kagkarlsson.scheduler.boot.config.DbSchedulerCustomizer;
import com.github.kagkarlsson.scheduler.task.Task;
import com.github.kagkarlsson.scheduler.task.helper.Tasks;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Optional;
import java.util.UUID;

import static com.github.kagkarlsson.scheduler.task.schedule.Schedules.cron;

@Slf4j
@Configuration
public class TaskConfiguration {

    private static final String SYNC_USER_LDAP_TASK = "sync-user-ldap";

    private final SyncService syncService;

    @Value("${app.job.sync-user-ldap.time-cron}")
    private String timeCron;

    public TaskConfiguration(SyncService syncService) {
        this.syncService = syncService;
    }

    @Bean
    Task<Void> syncUserLdapTask() {
        return Tasks
                .recurring(SYNC_USER_LDAP_TASK, cron(timeCron))
                .execute((instance, ctx) -> {
                    log.info("Starting sync user information from LDAP");
                    syncService.syncUserLdap();
                });
    }

    @Bean
    DbSchedulerCustomizer customizer() {
        return new DbSchedulerCustomizer() {
            @Override
            public Optional<SchedulerName> schedulerName() {
                return Optional.of(new SchedulerName.Fixed(String.format("%s-%s", "scheduler", UUID.randomUUID())));
            }
        };
    }
}

