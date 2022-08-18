/*
 * StringUtil.java
 *
 * Copyright (C) 2021 by Vinsmart. All right reserved.
 * This software is the confidential and proprietary information of Vinsmart
 */
package com.evotek.storage.infrastructure.support.util;

/**
 * The Class StringUtil.
 */
public class StringUtil {

    public static String getSafeFileName(String input) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            if (c != '/' && c != '\\' && c != 0) {
                sb.append(c);
            }
        }
        return sb.toString();
    }
}
