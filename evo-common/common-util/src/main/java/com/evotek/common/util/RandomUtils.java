package com.evotek.common.util;

import org.apache.commons.lang3.RandomStringUtils;

import java.security.SecureRandom;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

public final class RandomUtils {
    private static final int SEC_COUNT = 100;
    private static final int DEF_COUNT = 20;
    private static final int SALT_COUNT = 10;
    private static final String ID_DATETIME_PATTERN = "yyyyMMddSSSHHssmm";
    private static final AtomicLong LAST_TIME_MS = new AtomicLong();
    private static final Random RANDOM = new SecureRandom();

    private RandomUtils() {
        throw new IllegalStateException("Utility class");
    }

    public static String generateSecret() {
        return RandomStringUtils.randomAlphanumeric(SEC_COUNT);
    }

    public static String generatePassword() {
        return RandomStringUtils.randomAlphanumeric(DEF_COUNT);
    }

    public static String generateSalt() {
        return RandomStringUtils.randomAlphanumeric(SALT_COUNT);
    }

    public static String generateActivationKey() {
        return RandomStringUtils.randomNumeric(DEF_COUNT);
    }

    public static int generateRandom(int size) {
        return RANDOM.nextInt(size);
    }

    public static String generateResetKey() {
        return RandomStringUtils.randomNumeric(DEF_COUNT);
    }

    public static Long uniqueExternalID() {
        long now = System.currentTimeMillis();
        while (true) {
            long lastTime = LAST_TIME_MS.get();
            if (lastTime >= now)
                now = lastTime + 1;
            if (LAST_TIME_MS.compareAndSet(lastTime, now)) {
                return Long.valueOf(new SimpleDateFormat(ID_DATETIME_PATTERN, Locale.US).format(new Date(now)));
            }
        }
    }
}
