package com.evotek.notification.application.service;

public interface TelegramBotService {
    void sendMessage(String chatId, String message);

    void senMessageConfirmMeetingParticipation(String chatId, String message, String meetingId);

    void startTelegramBot();
}
