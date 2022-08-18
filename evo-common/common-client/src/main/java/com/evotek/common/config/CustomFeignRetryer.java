package com.evotek.common.config;

import feign.RetryableException;
import feign.Retryer;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.TimeUnit;

@Slf4j
public class CustomFeignRetryer implements Retryer {

    private final int maxAttempts;
    private final long backoff;
    int attempt;

    public CustomFeignRetryer() {
        this(TimeUnit.SECONDS.toMillis(5L), 3);
    }

    public CustomFeignRetryer(long backoff,
                              int maxAttempts) {
        this.backoff = backoff;
        this.maxAttempts = maxAttempts;
        this.attempt = 1;
    }

    @Override
    public void continueOrPropagate(RetryableException e) {

        if (attempt++ >= maxAttempts) {
            throw e;
        }

        try {
            TimeUnit.MILLISECONDS.sleep(backoff);
        } catch (InterruptedException ie) {
            log.error("InterruptedException: ", ie);
            Thread.currentThread().interrupt();
        }
        log.info("Retrying: " + e.request().url() + " attempt " + attempt);
    }

    @Override
    public Retryer clone() {
        return new CustomFeignRetryer(backoff, maxAttempts);
    }
}