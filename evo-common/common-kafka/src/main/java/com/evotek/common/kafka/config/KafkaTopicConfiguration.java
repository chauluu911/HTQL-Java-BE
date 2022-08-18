package com.evotek.common.kafka.config;

import com.evotek.common.kafka.support.KafkaTopic;
import org.apache.kafka.clients.admin.NewTopic;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.TopicBuilder;

import static com.evotek.common.kafka.support.KafkaConst.NUMBER_OF_PARTITION;
import static com.evotek.common.kafka.support.KafkaConst.NUMBER_OF_REPLICATE;

@Configuration
@ConditionalOnExpression("${spring.kafka.enabled:false}")
public class KafkaTopicConfiguration {

    /**
     * Để tạo một chủ đề (topic), chúng ta đăng ký một Bean NewTopic cho mỗi topic vào application context.
     * Nếu topic đã tồn tại, Bean sẽ bị bỏ qua.
     * Chúng ta có thể sử dụng TopicBuilder để tạo các Bean này.
     * KafkaAdmin cũng tăng số lượng phân vùng (partitions) nếu nó nhận thấy rằng một topic hiện có
     * có ít phân vùng hơn NewTopic.numPartitions.
     */
    @Bean
    public NewTopic actionLog() {
        return TopicBuilder.name(KafkaTopic.ACTION_LOG.getTopicName())
                .partitions(NUMBER_OF_PARTITION)
                .replicas(NUMBER_OF_REPLICATE).build();
    }
}
