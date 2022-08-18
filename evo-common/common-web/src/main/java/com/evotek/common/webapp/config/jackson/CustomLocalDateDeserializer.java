package com.evotek.common.webapp.config.jackson;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;

import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.UUID;

public class CustomLocalDateDeserializer extends JsonDeserializer<LocalDate> {
    @Override
    public LocalDate deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws
            IOException {
        try {
            return LocalDate.parse(jsonParser.getValueAsString(), DateTimeFormatter.ISO_LOCAL_DATE);
        } catch (Exception e) {
            throw deserializationContext.wrongTokenException(jsonParser,
                    UUID.class,
                    JsonToken.NOT_AVAILABLE,
                    "Attempted to parse String to LocalDate but this is forbidden");
        }
    }
}
