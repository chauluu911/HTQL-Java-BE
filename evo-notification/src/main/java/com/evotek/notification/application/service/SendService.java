package com.evotek.notification.application.service;

import com.evotek.notification.domain.Event;

public interface SendService {

    void sendNotification(String eventId);

    void sendViaEmail(Event event);

    void sendViaFirebase(Event event);

    void sendViaTelegram(Event event);

    void sendViaSms(Event event);

}
