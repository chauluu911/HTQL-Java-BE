package com.evotek.meet.application.service;

public interface ThymeleafService {
    String getContent(String templateName, String meetingId, String organizer);
}
