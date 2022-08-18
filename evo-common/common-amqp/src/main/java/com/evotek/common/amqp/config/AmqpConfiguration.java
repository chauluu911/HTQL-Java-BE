package com.evotek.common.amqp.config;

import com.evotek.common.amqp.support.ProtoStuffMessageConverter;
import com.evotek.common.amqp.support.QueueBindingConfig;
import com.evotek.common.util.MapperFactoryUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.amqp.core.Binding;
import org.springframework.amqp.core.BindingBuilder;
import org.springframework.amqp.core.DirectExchange;
import org.springframework.amqp.core.Queue;
import org.springframework.amqp.rabbit.annotation.EnableRabbit;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.Map;

import static com.evotek.common.amqp.support.QueueBindingConfig.ActionLog.EXCHANGE_RETRY_NAME;

@EnableRabbit
@Configuration
@ConditionalOnExpression("${spring.rabbitmq.enabled:false}")
public class AmqpConfiguration {

    @Bean
    public DirectExchange actionLogExchange() {
        return new DirectExchange(QueueBindingConfig.ActionLog.EXCHANGE_NAME);
    }

    @Bean
    public DirectExchange actionLogRetryExchange() {
        return new DirectExchange(EXCHANGE_RETRY_NAME, true, false);
    }

    @Bean
    public Queue actionLogQueue() {
        Map<String, Object> arguments = new HashMap<>();
//        arguments.put(QueueBindingConfig.RabbitQueueOption.DEAD_LETTER_EXCHANGE, QueueBindingConfig.ActionLog.EXCHANGE_DLQ_NAME);
//        arguments.put(QueueBindingConfig.RabbitQueueOption.DEAD_LETTER_ROUTING_KEY, QueueBindingConfig.ActionLog.DLQ_ROUTING_KEY);
        return new Queue(QueueBindingConfig.ActionLog.QUEUE_NAME,
                true,
                false,
                false, arguments);
    }

    @Bean
    public Queue actionLogQueueRetry() {
        Map<String, Object> arguments = new HashMap<>();
        arguments.put(QueueBindingConfig.RabbitQueueOption.DEAD_LETTER_EXCHANGE, QueueBindingConfig.ActionLog.EXCHANGE_NAME);
        arguments.put(QueueBindingConfig.RabbitQueueOption.DEAD_LETTER_ROUTING_KEY, QueueBindingConfig.ActionLog.ROUTING_KEY);
        arguments.put(QueueBindingConfig.RabbitQueueOption.MESSAGE_TTL, 3000); // retry delay sau 3s
        return new Queue(QueueBindingConfig.ActionLog.QUEUE_RETRY_NAME,
                true,
                false,
                false, arguments);
    }

    @Bean
    public Binding bindingActionLogDlq() {
        return BindingBuilder
                .bind(actionLogQueueRetry())
                .to(actionLogRetryExchange())
                .with(QueueBindingConfig.ActionLog.RETRY_ROUTING_KEY);
    }

    @Bean
    public Binding actionLogBinding() {
        return BindingBuilder
                .bind(actionLogQueue())
                .to(actionLogExchange())
                .with(QueueBindingConfig.ActionLog.ROUTING_KEY);
    }

    @Bean
    public RabbitTemplate rabbitTemplate(final ConnectionFactory connectionFactory) {
        final var rabbitTemplate = new RabbitTemplate(connectionFactory);
        rabbitTemplate.setMessageConverter(producerJackson2MessageConverter());
        return rabbitTemplate;
    }

    @Bean
    public Jackson2JsonMessageConverter producerJackson2MessageConverter() {
        ObjectMapper objectMapper = MapperFactoryUtil.jacksonMapper();
        return new Jackson2JsonMessageConverter(objectMapper);
    }

    //    @Bean
    public ProtoStuffMessageConverter protoStuffMessageConverter() {
        return new ProtoStuffMessageConverter();
    }
}
