package com.evotek.common.kafka.support;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.support.SendResult;
import org.springframework.util.concurrent.FailureCallback;
import org.springframework.util.concurrent.ListenableFuture;
import org.springframework.util.concurrent.SuccessCallback;

public abstract class AbstractKafkaProducerService {
    private static final Logger log = LoggerFactory.getLogger(AbstractKafkaProducerService.class);
    private final KafkaTemplate<String, String> kafkaTemplate;

    protected AbstractKafkaProducerService(KafkaTemplate<String, String> kafkaTemplate) {
        this.kafkaTemplate = kafkaTemplate;
    }

    public void send(String topicName, String data, SuccessCallback<SendResult<String, String>> successCallback, FailureCallback failureCallback) {
        log.info("Kafka send message : {} - {}", topicName, data);
        ListenableFuture<SendResult<String, String>> result = kafkaTemplate.send(topicName, data);
        result.addCallback(r -> {
            log.info("Kafka send success full : {} - {} - {} ", topicName, data, r);
            if (successCallback != null) {
                successCallback.onSuccess(r);
            }
        }, ex -> {
            log.error("kafka send fail : {} - {} - {}", topicName, data, ex);
            if (failureCallback != null) {
                failureCallback.onFailure(ex);
            }
        });
    }

    public void send(String topicName, String data, SuccessCallback<SendResult<String, String>> successCallback) {
        send(topicName, data, successCallback, null);
    }

    public void send(String topicName, String data) {
        send(topicName, data, null, null);
    }

    public SendResult<String, String> sendSync(String topicName, String data) throws Exception {
        ListenableFuture<SendResult<String, String>> future = kafkaTemplate.send(topicName, data);
        var result = future.completable();
        return result.get();
    }
}
