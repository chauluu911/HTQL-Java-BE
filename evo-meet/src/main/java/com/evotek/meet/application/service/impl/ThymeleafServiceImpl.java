package com.evotek.meet.application.service.impl;

import com.evotek.common.util.StringPool;
import com.evotek.meet.application.service.ThymeleafService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.thymeleaf.context.Context;
import org.thymeleaf.spring5.SpringTemplateEngine;

import java.util.Locale;

@Service
@Slf4j
@RequiredArgsConstructor
public class ThymeleafServiceImpl implements ThymeleafService {

    private static final String BASE_URL = "baseUrl";
    private static final String URL = "url";
    private static final String ORGANIZER = "organizer";

    private static final String REDIRECT_LINK_ACCEPT_MEETING = "/meetings";

    private final SpringTemplateEngine templateEngine;

    @Value("${app.domain}")
    private String domain;

    @Override
    public String getContent(String templateName, String meetingId, String organizer) {
        Locale locale = Locale.forLanguageTag("vi");
        String linkRedirect = String.format("%s%s", domain, REDIRECT_LINK_ACCEPT_MEETING);
        Context context = new Context(locale);
        context.setVariable(BASE_URL, domain);
        context.setVariable(ORGANIZER, organizer);
        context.setVariable(URL, linkRedirect + StringPool.FORWARD_SLASH + meetingId + StringPool.FORWARD_SLASH + "detail");
        return templateEngine.process(templateName, context);
    }
}
