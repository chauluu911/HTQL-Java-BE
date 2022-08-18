package com.evotek.common.webapp.config.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;

import java.io.IOException;

public class CustomStringDeserializer extends JsonDeserializer<String> {

    @Override
    public String deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
        if (jsonParser.getCurrentToken() == JsonToken.VALUE_NUMBER_INT) {
            throw deserializationContext.wrongTokenException(jsonParser,
                    String.class,
                    JsonToken.VALUE_STRING,
                    "Attempted to parse integer to string but this is forbidden");
        }
        if (jsonParser.getCurrentToken() == JsonToken.VALUE_TRUE || jsonParser.getCurrentToken() == JsonToken.VALUE_FALSE) {
            throw deserializationContext.wrongTokenException(jsonParser,
                    String.class,
                    JsonToken.VALUE_STRING,
                    "Attempted to parse boolean to string but this is forbidden");
        }
        var value = jsonParser.getValueAsString();
        return value == null ? null : value.trim();
    }
}
