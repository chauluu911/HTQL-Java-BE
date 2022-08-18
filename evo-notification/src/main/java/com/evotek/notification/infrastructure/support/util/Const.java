package com.evotek.notification.infrastructure.support.util;

import java.util.List;

public final class Const {

    public static final String DELIMITER = ",";
    public static final String TELEGRAM_SEND_MESSAGE = "[Telegram Bot] Sent message \"{}\" to {}";
    public static final String TELEGRAM_FAILED_SEND_MESSAGE ="[Telegram Bot] Failed to send message \"{}\" to {} due to error: {}";

    public static final String SLASH = "/";

    public static final List<String> WHITELIST_FILE_TYPES = List.of("application/pdf");

    public static final String LOGO_PATH = "images/logo.jpg";

    public static final String LOGO_CONTENT_ID = "logoContentId";

}
