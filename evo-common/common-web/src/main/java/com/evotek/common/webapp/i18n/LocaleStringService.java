package com.evotek.common.webapp.i18n;

import java.util.Locale;

public interface LocaleStringService {
    Locale getCurrentLocale();

    String getMessage(String messageCode, String defaultMessage, Object... params);
}
