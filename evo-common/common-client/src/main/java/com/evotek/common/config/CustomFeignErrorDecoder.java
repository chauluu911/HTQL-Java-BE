package com.evotek.common.config;

import com.evotek.common.dto.error.ErrorResponse;
import com.evotek.common.exception.ForwardInnerAlertException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import feign.Response;
import feign.RetryableException;
import feign.codec.ErrorDecoder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;

import java.io.IOException;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.Date;

@Slf4j
public class CustomFeignErrorDecoder implements ErrorDecoder {

    private final ErrorDecoder errorDecoder = new Default();

    @Autowired
    private ObjectMapper objectMapper;

    @Override
    public Exception decode(String methodKey, Response response) {
        Reader reader = null;

        try {
            if (response.status() == 503) {
                log.info("Error while executing " + methodKey + " Error code "
                        + HttpStatus.NOT_FOUND);
                return new RetryableException(response.status(), methodKey, null,
                        new Date(System.currentTimeMillis()),
                        response.request());
            }
            reader = response.body().asReader(StandardCharsets.UTF_8);
            ErrorResponse<Void> exceptionMessage = objectMapper.readValue(reader,
                    new TypeReference<>() {
                    });
            return new ForwardInnerAlertException(exceptionMessage);

        } catch (IOException e) {
            log.error("Feign decode error", e);
        } finally {
            try {
                if (reader != null)
                    reader.close();
            } catch (IOException e) {
                log.error("Feign decode error - ignore", e);
            }
        }
        return errorDecoder.decode(methodKey, response);
    }
}
