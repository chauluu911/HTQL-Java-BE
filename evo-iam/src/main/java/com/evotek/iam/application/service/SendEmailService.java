package com.evotek.iam.application.service;

import com.evotek.iam.domain.User;

import javax.mail.MessagingException;

public interface SendEmailService {

    void send(User user, String templateName, String titleKey, String token) throws MessagingException;
}
