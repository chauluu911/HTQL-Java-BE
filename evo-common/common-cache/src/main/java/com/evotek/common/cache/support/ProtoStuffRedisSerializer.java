package com.evotek.common.cache.support;

import com.evotek.common.util.SerializationUtil;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.SerializationException;
import org.springframework.lang.Nullable;

public class ProtoStuffRedisSerializer implements RedisSerializer<Object> {

    static final byte[] EMPTY_ARRAY = new byte[0];

    static boolean isEmpty(@Nullable byte[] data) {
        return data == null || data.length == 0;
    }

    public Object deserialize(@Nullable byte[] bytes) throws SerializationException {
        if (isEmpty(bytes)) {
            return null;
        } else {
            try {
                ObjectWrapper objectWrapper = SerializationUtil.deserializeFromByte(bytes, ObjectWrapper.class);
                return objectWrapper.getTarget();
            } catch (Exception var3) {
                throw new SerializationException("Could not read binary: " + var3.getMessage(), var3);
            }
        }
    }

    public byte[] serialize(@Nullable Object t) throws SerializationException {
        if (t == null) {
            return EMPTY_ARRAY;
        } else {
            ObjectWrapper wrapper = new ObjectWrapper(t);
            try {
                return SerializationUtil.serializeToByte(wrapper);
            } catch (Exception var3) {
                throw new SerializationException("Could not write binary: " + var3.getMessage(), var3);
            }
        }
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    private static class ObjectWrapper {

        private Object target;
    }
}
