package com.evotek.common.kafka.support;

public enum KafkaTopic {

    ACTION_LOG("evotek.action_log");

    private final String topicName;

    KafkaTopic(String s) {
        this.topicName = s;
    }

    public String getTopicName() {
        return topicName;
    }
}
