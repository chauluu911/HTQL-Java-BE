package com.evotek.meet.application.config;

import com.evotek.meet.application.service.SendService;
import com.evotek.meet.domain.UserSchedulerApproveTaskData;
import com.evotek.meet.domain.UserSchedulerRejectOrCancelTaskData;
import com.evotek.meet.infrastructure.support.Util.JobConst;
import com.github.kagkarlsson.scheduler.SchedulerName;
import com.github.kagkarlsson.scheduler.boot.config.DbSchedulerCustomizer;
import com.github.kagkarlsson.scheduler.task.DeadExecutionHandler;
import com.github.kagkarlsson.scheduler.task.Task;
import com.github.kagkarlsson.scheduler.task.helper.Tasks;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Optional;
import java.util.UUID;

@Configuration
@AutoConfigureAfter({SendService.class})
@Slf4j
public class TaskConfiguration {
    private static final String SEND_USER_SCHEDULER_TASK = "send-user-scheduler";
    private final SendService sendService;

    public TaskConfiguration(SendService sendService) {
        this.sendService = sendService;
    }

    @Bean
    Task<String> sendMailTask() {
        return Tasks.oneTime(JobConst.SEND_MAIL_TASK, String.class)
                .onDeadExecution(new DeadExecutionHandler.ReviveDeadExecution<>())   // re-run task which was dead when shutdown server
                .execute(((taskInstance, executionContext) -> {
                    log.info("Start execution with task name: " + taskInstance.getTaskName()
                            + " with task instance: " + taskInstance.getTaskAndInstance());
                    String eventId = taskInstance.getData();
                    sendService.sendMail(eventId);
                }));
    }

    @Bean
    Task<String> sendNotificationTask() {
        return Tasks.oneTime(JobConst.SEND_NOTIFICATION_TASK, String.class)
                .onDeadExecution(new DeadExecutionHandler.ReviveDeadExecution<>())   // re-run task which was dead when shutdown server
                .execute(((taskInstance, executionContext) -> {
                    log.info("Start execution with task name: " + taskInstance.getTaskName()
                            + " with task instance: " + taskInstance.getTaskAndInstance());
                    String eventId = taskInstance.getData();
                    sendService.sendNotification(eventId);
                }));
    }
    @Bean
    Task<String> sendUserSchedulerTask() {
        return Tasks.oneTime(JobConst.SEND_USER_SCHEDULER_TASK, String.class)
                .onDeadExecution(new DeadExecutionHandler.ReviveDeadExecution<>())   // re-run task which was dead when shutdown server
                .execute(((taskInstance, executionContext) -> {
                    log.info("Start execution with task name: " + taskInstance.getTaskName()
                            + " with task instance: " + taskInstance.getTaskAndInstance());
                    String eventId = taskInstance.getData();
                    sendService.sendUserScheduler(eventId);
                }));
    }

    @Bean
    Task<UserSchedulerRejectOrCancelTaskData> sendRejectOrCancelUserSchedulerTask() {
        return Tasks.oneTime(JobConst.SEND_REJECT_USER_SCHEDULER_TASK, UserSchedulerRejectOrCancelTaskData.class)
                .onDeadExecution(new DeadExecutionHandler.ReviveDeadExecution<>())   // re-run task which was dead when shutdown server
                .execute(((taskInstance, executionContext) -> {
                    log.info("Start execution with task name: " + taskInstance.getTaskName()
                            + " with task instance: " + taskInstance.getTaskAndInstance());
                    UserSchedulerRejectOrCancelTaskData data = taskInstance.getData();
                    sendService.sendRejectOrCancelUserScheduler(data);
                }));
    }

    @Bean
    Task<UserSchedulerApproveTaskData> sendApproveUserSchedulerTask() {
        return Tasks.oneTime(JobConst.SEND_APPROVE_USER_SCHEDULER_TASK, UserSchedulerApproveTaskData.class)
                .onDeadExecution(new DeadExecutionHandler.ReviveDeadExecution<>())   // re-run task which was dead when shutdown server
                .execute(((taskInstance, executionContext) -> {
                    log.info("Start execution with task name: " + taskInstance.getTaskName()
                            + " with task instance: " + taskInstance.getTaskAndInstance());
                    UserSchedulerApproveTaskData data = taskInstance.getData();
                    sendService.sendApproveUserScheduler(data);
                }));
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
