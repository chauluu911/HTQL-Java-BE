package com.evotek.common.util;

import java.util.Arrays;
import java.util.List;

public interface Constants {

    String ANONYMOUS_ACCOUNT = "anonymous";

    String DOT = ".";

    List<String> EXTENSIONS = Arrays.asList(("bmp,jpg,png,jpeg".split(",")));
    String AUTHORITY_TYPE = "auth_type";
    String USER_ID = "user_id";
    String EMAIL = "email";
    String CLIENT = "client";
    String REFRESH_TOKEN = "refresh_token";
    String CLIENT_AUTHORITY = "client";

    static List<String> getValidExtensions() {
        return EXTENSIONS;
    }
}
