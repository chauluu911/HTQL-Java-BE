package com.evotek.common.persistence.support;

import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StringUtils;

import java.util.Objects;

@Slf4j
public class SqlUtils {
    public static final String PERCENT = "%";

    private SqlUtils() {
        throw new IllegalStateException("Utility class");
    }

    public static String replaceSpecialCharacter(String input) {
        if (StringUtils.hasLength(input)) {
            return input.trim().replace("%", "\\%").replace("_", "\\_").toLowerCase();
        }
        return "";
    }

    public static String encodeKeyword(String param) {
        if (Objects.isNull(param)) {
            return null;
        }
        StringBuilder sb = new StringBuilder(5);
        sb.append(PERCENT);
        sb.append(replaceSpecialCharacter(param));
        sb.append(PERCENT);
        return sb.toString();
    }
}
