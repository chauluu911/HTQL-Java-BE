package com.evotek.common.webapp.config.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;

import java.io.IOException;

public class CustomLongDeserializer extends JsonDeserializer<Long> {

    @Override
    public Long deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws
            IOException {
        if (jsonParser.getCurrentToken() != JsonToken.VALUE_NUMBER_INT) {
            throw deserializationContext.wrongTokenException(jsonParser,
                    Long.class,
                    JsonToken.VALUE_NUMBER_INT,
                    "Attempted to parse non-Int value to Long but this is forbidden");
        }
        return jsonParser.getValueAsLong();
    }
}
