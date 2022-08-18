package com.evotek.common.cache;

import com.evotek.common.cache.support.PageSerializer;
import com.evotek.common.cache.support.ProtoStuffRedisSerializer;
import com.evotek.common.util.MapperFactoryUtil;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.datatype.hibernate5.Hibernate5Module;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.cache.CacheProperties;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.data.redis.RedisAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cache.annotation.CachingConfigurerSupport;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.interceptor.CacheErrorHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.cache.RedisCacheWriter;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.serializer.Jackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.RedisSerializationContext;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

@Slf4j
@EnableCaching
@Configuration
@AutoConfigureAfter(RedisAutoConfiguration.class)
@EnableConfigurationProperties({CacheProperties.class, CustomCacheProperties.class})
@ConditionalOnClass({CacheProperties.Redis.class, RedisCacheConfiguration.class})
public class RedisCacheAutoConfiguration extends CachingConfigurerSupport {

    private final CacheProperties cacheProperties;

    public RedisCacheAutoConfiguration(CacheProperties cacheProperties) {
        this.cacheProperties = cacheProperties;
    }

    @Bean
    public RedisCacheManager redisCacheManager(CustomCacheProperties customCacheProperties,
                                               RedisConnectionFactory redisConnectionFactory) {
        RedisCacheConfiguration defaultConfiguration = getDefaultRedisCacheConfiguration();
//        RedisCacheConfiguration defaultConfiguration = getProtoStuffRedisCacheConfiguration();

        RedisCacheManager.RedisCacheManagerBuilder builder = RedisCacheManager.RedisCacheManagerBuilder
                .fromCacheWriter(RedisCacheWriter.nonLockingRedisCacheWriter(redisConnectionFactory))
                .cacheDefaults(defaultConfiguration);

        Map<String, RedisCacheConfiguration> map = new HashMap<>();
        Optional.ofNullable(customCacheProperties)
                .map(CustomCacheProperties::getCustomCache)
                .ifPresent(customCache -> customCache.forEach((key, cache) -> {
                    RedisCacheConfiguration cfg = handleRedisCacheConfiguration(cache, defaultConfiguration);
                    map.put(key, cfg);
                }));
        builder.withInitialCacheConfigurations(map);
        return builder.build();
    }

    @Override
    public CacheErrorHandler errorHandler() {
        return new RedisCacheErrorHandler();
    }

    private RedisCacheConfiguration getDefaultRedisCacheConfiguration() {
        CacheProperties.Redis redisProperties = cacheProperties.getRedis();
        RedisCacheConfiguration config = RedisCacheConfiguration.defaultCacheConfig();

        Jackson2JsonRedisSerializer<Object> jackson2JsonRedisSerializer = getJackson2JsonRedisSerializer();
        config = config.serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(new StringRedisSerializer()));
        config = config.serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(jackson2JsonRedisSerializer));
        config = handleRedisCacheConfiguration(redisProperties, config);
        return config;
    }

    private RedisCacheConfiguration getProtoStuffRedisCacheConfiguration() {
        CacheProperties.Redis redisProperties = cacheProperties.getRedis();
        RedisCacheConfiguration config = RedisCacheConfiguration.defaultCacheConfig();

        ProtoStuffRedisSerializer protoStuffRedisSerializer = protoStuffRedisSerializer();
        config = config.serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(new StringRedisSerializer()));
        config = config.serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(protoStuffRedisSerializer));
        config = handleRedisCacheConfiguration(redisProperties, config);
        return config;
    }

    private ProtoStuffRedisSerializer protoStuffRedisSerializer() {
        return new ProtoStuffRedisSerializer();
    }

    private Jackson2JsonRedisSerializer<Object> getJackson2JsonRedisSerializer() {
        Jackson2JsonRedisSerializer<Object> jackson2JsonRedisSerializer = new Jackson2JsonRedisSerializer<>(Object.class);
        ObjectMapper om = MapperFactoryUtil.jacksonMapper();
        om.registerModules(new SimpleModule().addSerializer(PageImpl.class, new PageSerializer()),
                new Hibernate5Module()
        );
        jackson2JsonRedisSerializer.setObjectMapper(om);
        return jackson2JsonRedisSerializer;
    }

    private RedisCacheConfiguration handleRedisCacheConfiguration(CacheProperties.Redis redisProperties, RedisCacheConfiguration config) {
        if (Objects.isNull(redisProperties)) {
            return config;
        }

        if (redisProperties.getTimeToLive() != null) {
            config = config.entryTtl(redisProperties.getTimeToLive());
        }
        if (redisProperties.getKeyPrefix() != null) {
            config = config.computePrefixWith(cacheName -> cacheName + redisProperties.getKeyPrefix());
        }
        if (!redisProperties.isCacheNullValues()) {
            config = config.disableCachingNullValues();
        }
        if (!redisProperties.isUseKeyPrefix()) {
            config = config.disableKeyPrefix();
        }
        return config;
    }
}