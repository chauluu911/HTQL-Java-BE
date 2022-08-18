package com.evotek.common.webapp.config.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;

import java.io.IOException;

public class CustomIntegerDeserializer extends JsonDeserializer<Integer> {
    @Override
    public Integer deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws
            IOException {
        if (jsonParser.getCurrentToken() != JsonToken.VALUE_NUMBER_INT) {
            throw deserializationContext.wrongTokenException(jsonParser,
                    Integer.class,
                    JsonToken.VALUE_NUMBER_INT,
                    "Attempted to parse non-Int value to Integer but this is forbidden");
        }
        return jsonParser.getValueAsInt();
    }
}