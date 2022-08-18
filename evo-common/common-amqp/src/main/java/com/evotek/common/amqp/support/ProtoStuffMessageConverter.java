package com.evotek.common.amqp.support;

import com.evotek.common.util.SerializationUtil;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.core.MessageProperties;
import org.springframework.amqp.support.converter.*;
import org.springframework.lang.Nullable;
import org.springframework.util.MimeType;
import org.springframework.util.MimeTypeUtils;

import java.lang.reflect.Type;

public class ProtoStuffMessageConverter extends AbstractMessageConverter
        implements SmartMessageConverter {
    protected final Log log = LogFactory.getLog(this.getClass());
    private final MimeType supportedContentType = MimeTypeUtils.parseMimeType("application/binary+protostuff");
    private final DefaultClassMapper classMapper;

    public ProtoStuffMessageConverter() {
        this.classMapper = new DefaultClassMapper();
        this.classMapper.setTrustedPackages("*");
    }

    @Nullable
    public ClassMapper getClassMapper() {
        return this.classMapper;
    }

    @Override
    public Object fromMessage(Message message) throws MessageConversionException {
        return this.fromMessage(message, (Object) null);
    }

    @Override
    public Object fromMessage(Message message, @Nullable Object conversionHint) throws MessageConversionException {
        Object content = null;
        MessageProperties properties = message.getMessageProperties();
        if (properties != null) {
            String contentType = properties.getContentType();
            if (contentType != null
                    && contentType.contains(this.supportedContentType.getSubtype())) {
                content = this.doFromMessage(message, properties);
            } else if (this.log.isWarnEnabled()) {
                this.log.warn("Could not convert incoming message with content-type [" + contentType + "], '"
                        + this.supportedContentType.getSubtype() + "' keyword missing.");
            }
        }
        if (content == null) {
            content = message.getBody();
        }

        return content;
    }

    private Object doFromMessage(Message message, MessageProperties properties) {
        Object content;
        if (this.classMapper != null) {
            Class<?> targetClass = this.classMapper.toClass(properties);
            content = SerializationUtil.deserializeFromByte(message.getBody(), targetClass);
        } else {
            content = message.getBody();
        }

        return content;
    }

    @Override
    public Message createMessage(Object objectToConvert,
                                 MessageProperties messageProperties) throws MessageConversionException {
        return this.createMessage(objectToConvert, messageProperties, (Type) null);
    }

    @Override
    public Message createMessage(Object objectToConvert,
                                 MessageProperties messageProperties,
                                 @Nullable Type genericType) throws MessageConversionException {
        byte[] bytes = SerializationUtil.serializeToByte(objectToConvert);
        messageProperties.setContentType(this.supportedContentType.toString());
        messageProperties.setContentLength(bytes.length);
        if (this.classMapper != null) {
            this.classMapper.fromClass(objectToConvert.getClass(), messageProperties);
        }

        return new Message(bytes, messageProperties);
    }
}
