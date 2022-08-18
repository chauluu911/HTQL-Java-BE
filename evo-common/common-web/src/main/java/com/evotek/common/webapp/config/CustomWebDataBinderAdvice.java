package com.evotek.common.webapp.config;

import com.evotek.common.error.BadRequestError;
import com.evotek.common.exception.ResponseException;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.propertyeditors.StringTrimmerEditor;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.InitBinder;

import java.beans.PropertyEditorSupport;
import java.time.Instant;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.UUID;

@ControllerAdvice
@Log4j2
public class CustomWebDataBinderAdvice {

    private final String EPOCH_SPLIT = "\\.";

    @InitBinder
    public void initBinder(final WebDataBinder webdataBinder) {
        webdataBinder.registerCustomEditor(Instant.class, new PropertyEditorSupport() {
            @Override
            public void setAsText(String text) throws IllegalArgumentException {
                if (StringUtils.hasLength(text)) {
                    String[] parts = text.split(EPOCH_SPLIT);
                    if (parts.length == 1) {
                        setValue(Instant.ofEpochMilli(Long.parseLong(parts[0])));
                    } else {
                        setValue(Instant.ofEpochSecond(Long.parseLong(parts[0]), Long.parseLong(parts[1])));
                    }
                }
            }
        });

        StringTrimmerEditor stringTrimmerEditor = new StringTrimmerEditor(true);
        webdataBinder.registerCustomEditor(String.class, stringTrimmerEditor);

        webdataBinder.registerCustomEditor(Long.class, new PropertyEditorSupport() {
            @Override
            public void setAsText(String text) throws IllegalArgumentException {
                if (StringUtils.hasLength(text)) {
                    try {
                        setValue(Long.parseLong(text.trim()));
                    } catch (Exception e) {
                        log.error(e.getMessage(), e);
                        throw new ResponseException(BadRequestError.INVALID_INPUT.getMessage(), BadRequestError.INVALID_INPUT, e.getMessage());
                    }
                }
            }
        });

        webdataBinder.registerCustomEditor(Integer.class, new PropertyEditorSupport() {
            @Override
            public void setAsText(String text) throws IllegalArgumentException {
                if (StringUtils.hasLength(text)) {
                    try {
                        setValue(Integer.parseInt(text.trim()));
                    } catch (Exception e) {
                        log.error(e.getMessage(), e);
                        throw new ResponseException(BadRequestError.INVALID_INPUT.getMessage(), BadRequestError.INVALID_INPUT, e.getMessage());
                    }
                }
            }
        });

        webdataBinder.registerCustomEditor(UUID.class, new PropertyEditorSupport() {
            @Override
            public void setAsText(String text) throws IllegalArgumentException {
                if (StringUtils.hasLength(text)) {
                    try {
                        setValue(UUID.fromString(text.trim()));
                    } catch (Exception e) {
                        log.error(e.getMessage(), e);
                        throw new ResponseException(BadRequestError.INVALID_INPUT.getMessage(), BadRequestError.INVALID_INPUT, e.getMessage());
                    }

                }
            }
        });

        webdataBinder.registerCustomEditor(LocalDate.class, new PropertyEditorSupport() {
            @Override
            public void setAsText(String text) throws IllegalArgumentException {
                if (StringUtils.hasLength(text)) {
                    try {
                        var data = LocalDate.parse(text.trim(), DateTimeFormatter.ISO_LOCAL_DATE);
                        setValue(data);
                    } catch (Exception e) {
                        log.error(e.getMessage(), e);
                        throw new ResponseException(BadRequestError.INVALID_INPUT.getMessage(), BadRequestError.INVALID_INPUT, e.getMessage());
                    }
                }
            }
        });
    }
}
