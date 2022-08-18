package com.evotek.common.webapp.config.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;

import java.io.IOException;
import java.util.UUID;

public class CustomUUIDDeserializer extends JsonDeserializer<UUID> {
    @Override
    public UUID deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
        try {
            return UUID.fromString(jsonParser.getValueAsString());
        } catch (Exception e) {
            throw deserializationContext.wrongTokenException(jsonParser,
                    UUID.class,
                    JsonToken.NOT_AVAILABLE,
                    "Attempted to parse String to UUID but this is forbidden");
        }
    }
}
