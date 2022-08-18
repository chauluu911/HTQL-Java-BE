package com.evotek.common.kafka.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.kafka.KafkaProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.annotation.EnableKafka;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.*;
import org.springframework.kafka.listener.ContainerProperties;

import static com.evotek.common.kafka.support.KafkaConst.ACK_CONTAINER_FACTORY;
import static com.evotek.common.kafka.support.KafkaConst.REPLY_CONTAINER_FACTORY;

@Configuration
@EnableConfigurationProperties({KafkaProperties.class})
@EnableKafka
@ConditionalOnExpression("${spring.kafka.enabled:false}")
public class KafkaAutoConfiguration {

    private final int defaultPartition;

    /**
     * Read config from yaml file
     */
    private final KafkaProperties kafkaProperties;

    public KafkaAutoConfiguration(KafkaProperties kafkaProperties,
                                  @Value("${spring.kafka.consumer.default-partition:1}") int defaultPartition) {
        this.kafkaProperties = kafkaProperties;
        this.defaultPartition = defaultPartition;
    }

    /**
     * KafkaTemplate chịu trách nhiệm gửi tin nhắn đến các topic tương ứng
     * . Chúng ta sẽ tìm hiểu hơn về KafkaTemplate ở phần bên dưới.
     *
     * @return KafkaTemplate
     */
    @Bean
    public KafkaTemplate<String, String> kafkaTemplate() {
        return new KafkaTemplate<>(producerFactory());
    }

    /**
     * ProducerFactory chịu trách nhiệm tạo các instance Kafka Producer để gửi các object message khác nhau
     * (trong trường hợp trên object là String, chúng ta cũng có thể custom message dạng Java object do chúng ta định nghĩa
     *
     * @return ProducerFactory
     */
    @Bean
    public ProducerFactory<String, String> producerFactory() {
        return new DefaultKafkaProducerFactory<>(kafkaProperties.buildProducerProperties());
    }


    /**
     * ConsumerFactory chịu trách nhiệm tạo các instance Kafka consumer để nhận các object message khác nhau
     * (trong trường hợp trên object là String, chúng ta cũng có thể custom message dạng Java object do chúng ta định nghĩa
     */
    @Bean
    public ConsumerFactory<String, String> consumerFactory() {
        return new DefaultKafkaConsumerFactory<>(kafkaProperties.buildConsumerProperties());
    }

    /**
     * kafkaListenerContainerFactory sẽ nhận tất cả tin nhắn từ tất cả các topic hoặc partition trên một luồng duy nhất
     */
    @Bean
    public ConcurrentKafkaListenerContainerFactory<String, String> kafkaListenerContainerFactory() {
        ConcurrentKafkaListenerContainerFactory<String, String> factory = new ConcurrentKafkaListenerContainerFactory<>();
        factory.setConsumerFactory(consumerFactory());
        factory.setConcurrency(defaultPartition);
        factory.getContainerProperties().setAckMode(ContainerProperties.AckMode.RECORD);
        factory.setBatchListener(false);
        factory.getContainerProperties().setPollTimeout(3000);

        ContainerProperties containerProperties = factory.getContainerProperties();
        containerProperties.setMissingTopicsFatal(false);

        return factory;
    }

    /**
     * kafkaAckListenerContainerFactory sẽ nhận tất cả tin nhắn từ tất cả các topic hoặc partition trên một luồng duy nhất
     */
    @Bean(ACK_CONTAINER_FACTORY)
    public ConcurrentKafkaListenerContainerFactory<String, String> kafkaAckListenerContainerFactory() {
        ConcurrentKafkaListenerContainerFactory<String, String> factory = new ConcurrentKafkaListenerContainerFactory<>();
        factory.setConsumerFactory(consumerFactory());
        factory.getContainerProperties().setAckMode(ContainerProperties.AckMode.MANUAL_IMMEDIATE);
        factory.setConcurrency(defaultPartition);
        return factory;
    }

    /**
     * kafkaReplyListenerContainerFactory sẽ nhận tất cả tin nhắn từ tất cả các topic hoặc partition trên một luồng duy nhất
     */
    @Bean(REPLY_CONTAINER_FACTORY)
    public ConcurrentKafkaListenerContainerFactory<String, String> kafkaReplyListenerContainerFactory() {
        ConcurrentKafkaListenerContainerFactory<String, String> factory =
                new ConcurrentKafkaListenerContainerFactory<>();
        factory.setConsumerFactory(consumerFactory());
        factory.setBatchListener(true);
        factory.setReplyTemplate(kafkaTemplate()); // khi nhận được tin nhắn trả về, sẽ gửi lại cho producer
        return factory;
    }
}
