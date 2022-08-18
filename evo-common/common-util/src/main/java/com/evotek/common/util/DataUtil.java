package com.evotek.common.util;

import java.util.Objects;

public final class DataUtil {
    public static <T> T getValueOrDefault(T value, T defaultValue) {
        return Objects.isNull(value) ? defaultValue : value;
    }
}
