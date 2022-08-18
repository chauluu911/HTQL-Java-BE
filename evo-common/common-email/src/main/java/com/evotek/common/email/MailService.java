package com.evotek.common.email;

import javax.mail.MessagingException;
import java.util.List;

public interface MailService {

    void sendSimpleMail(String to, String subject, String content, String... cc);

    void sendHtmlMail(String to, String subject, String content, String... cc) throws MessagingException;

    void sendAttachmentsMail(String to, String subject, String content, String filePath, String... cc) throws MessagingException;

    void sendAttachmentsMailBcc(List<String> bcc, String subject, String content,
                                List<String> filePaths, List<String> fileNames,
                                String logoPath, String logoContentId,
                                String... cc) throws MessagingException;

    void sendResourceMail(String to, String subject, String content, String rscPath, String rscId, String... cc) throws MessagingException;
}
