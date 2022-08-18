package com.evotek.notification.application.config;

import com.evotek.notification.application.service.SendService;
import com.evotek.notification.application.service.TelegramBotService;
import com.evotek.notification.infrastructure.support.util.JobConst;
import com.github.kagkarlsson.scheduler.SchedulerName;
import com.github.kagkarlsson.scheduler.boot.config.DbSchedulerCustomizer;
import com.github.kagkarlsson.scheduler.task.DeadExecutionHandler;
import com.github.kagkarlsson.scheduler.task.Task;
import com.github.kagkarlsson.scheduler.task.helper.Tasks;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Optional;
import java.util.UUID;

import static com.github.kagkarlsson.scheduler.task.schedule.Schedules.cron;

@Configuration
@AutoConfigureAfter({SendService.class, TelegramBotService.class})
@Slf4j
public class TaskConfiguration {

    private final SendService sendService;
    private final TelegramBotService telegramBotService;
    @Value("${app.job.start-polling-telegram-bot}")
    private String cronExpression;

    public TaskConfiguration(SendService sendService,
                             TelegramBotService telegramBotService) {
        this.sendService = sendService;
        this.telegramBotService = telegramBotService;
    }

    @Bean
    Task<String> manuallySendEventTask() {
        return Tasks.oneTime(JobConst.SEND_EVENT_TASK, String.class)
                .onDeadExecution(new DeadExecutionHandler.ReviveDeadExecution<>())   // re-run task which was dead when shutdown server
                .execute(((taskInstance, executionContext) -> {
                    log.info("Start execution with task name: " + taskInstance.getTaskName()
                            + " with task instance: " + taskInstance.getTaskAndInstance());
                    String eventId = taskInstance.getData();
                    sendService.sendNotification(eventId);
                }));
    }

    @Bean
    Task<Void> syncTask() {
        return Tasks
                .recurring(JobConst.POLLING_UPDATE_RECEIVED_TELEGRAM, cron(cronExpression))
                .execute((taskInstance, executionContext) -> {
                    log.info("Start execution recurring with task name: " + taskInstance.getTaskName()
                            + " with task instance: " + taskInstance.getTaskAndInstance());
                    telegramBotService.startTelegramBot();
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
