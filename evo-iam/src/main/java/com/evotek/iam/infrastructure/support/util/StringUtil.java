package com.evotek.iam.infrastructure.support.util;

import java.text.Normalizer;
import java.util.regex.Pattern;

/**
 * Utility class for generating random Strings.
 */
public final class StringUtil {

    private static final String REGEX_PASSWORD = "^(?=.*\\d)(?=.*[a-z])(?=.*[A-Z])(?=.*[a-zA-Z]).{8,}$";

    private StringUtil() {
    }

    public static String removeAccent(String s) {
        String temp = Normalizer.normalize(s, Normalizer.Form.NFD);
        Pattern pattern = Pattern.compile("\\p{InCombiningDiacriticalMarks}+");
        return pattern.matcher(temp).replaceAll("");
    }

    public static boolean validatePassword(String password) {
        return Pattern.compile(REGEX_PASSWORD).matcher(password).matches();
    }
}
