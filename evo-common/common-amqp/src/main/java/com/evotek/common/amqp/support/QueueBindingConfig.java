package com.evotek.common.amqp.support;

public final class QueueBindingConfig {
    private QueueBindingConfig() {
    }

    public interface RabbitQueueOption {
        String MESSAGE_TTL = "x-message-ttl"; // ttl is the time to live of the message in milliseconds
        String AUTO_EXPIRE = "x-expires";
        String MAX_LENGTH_BYTE = "x-max-length-bytes";
        String DEAD_LETTER_EXCHANGE = "x-dead-letter-exchange"; //exchange-name is the name of the exchange to which messages will be forwarded if they are rejected
        String DEAD_LETTER_ROUTING_KEY = "x-dead-letter-routing-key"; // routing-key is the name of the routing key to which messages will be forwarded if they are rejected
    }

    public static final class ActionLog {
        public static final String EXCHANGE_NAME = "evotek.action_log.exchange";
        public static final String EXCHANGE_RETRY_NAME = "evotek.action_log_retry.exchange";
        public static final String QUEUE_NAME = "evotek.action_log.queue";
        public static final String QUEUE_RETRY_NAME = "evotek.action_log_retry.queue";
        public static final String ROUTING_KEY = "evotek.action_log.key";
        public static final String RETRY_ROUTING_KEY = "evotek.action_log_retry.key";

        private ActionLog() {
        }
    }
}
