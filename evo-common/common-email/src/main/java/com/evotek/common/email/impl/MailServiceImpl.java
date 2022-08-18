package com.evotek.common.email.impl;

import com.evotek.common.email.MailService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.thymeleaf.util.ArrayUtils;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;
import java.io.File;
import java.util.List;
import java.util.Objects;

@Service
public class MailServiceImpl implements MailService {

    @Value("classpath:/mail/mail-logo.png")
    Resource resourceFile;

    @Autowired
    private JavaMailSender mailSender;
    @Value("${spring.mail.username}")
    private String from;

    @Override
    public void sendSimpleMail(String to, String subject, String content, String... cc) {
        SimpleMailMessage message = new SimpleMailMessage();
        message.setFrom(from);
        message.setTo(to);
        message.setSubject(subject);
        message.setText(content);
        if (!ArrayUtils.isEmpty(cc)) {
            message.setCc(cc);
        }
        mailSender.send(message);
    }

    @Override
    public void sendHtmlMail(String to, String subject, String content, String... cc) throws MessagingException {
        MimeMessage message = mailSender.createMimeMessage();
        MimeMessageHelper helper = new MimeMessageHelper(message, true);
        helper.setFrom(from);
        helper.setTo(to);
        helper.setSubject(subject);
        helper.setText(content, true);
        if (!ArrayUtils.isEmpty(cc)) {
            helper.setCc(cc);
        }
        mailSender.send(message);
    }

    @Override
    public void sendAttachmentsMail(String to, String subject, String content, String filePath, String... cc) throws MessagingException {
        MimeMessage message = mailSender.createMimeMessage();

        MimeMessageHelper helper = new MimeMessageHelper(message, true);
        helper.setFrom(from);
        helper.setTo(to);
        helper.setSubject(subject);
        helper.setText(content, true);
        if (!ArrayUtils.isEmpty(cc)) {
            helper.setCc(cc);
        }
        FileSystemResource file = new FileSystemResource(new File(filePath));
        String fileName = filePath.substring(filePath.lastIndexOf(File.separator));
        helper.addAttachment(fileName, file);

        mailSender.send(message);
    }

    @Override
    public void sendAttachmentsMailBcc(List<String> bcc, String subject, String content,
                                       List<String> filePaths, List<String> fileNames,
                                       String logoPath, String logoContentId,
                                       String... cc) throws MessagingException {
        MimeMessage message = mailSender.createMimeMessage();
        MimeMessageHelper helper = new MimeMessageHelper(message, true);
        helper.setFrom(from);
        helper.setSubject(subject);
        helper.setText(content, true);
        if (!CollectionUtils.isEmpty(bcc)) {
            helper.setBcc(bcc.toArray(new String[0]));
        }
        if (!CollectionUtils.isEmpty(filePaths) && !CollectionUtils.isEmpty(fileNames) && filePaths.size() == fileNames.size()) {
            int i = 0;
            for (String filePath : filePaths) {
                FileSystemResource file = new FileSystemResource(new File(filePath));
                helper.addAttachment(fileNames.get(i), file);
                i++;
            }
        }
        if (Objects.nonNull(logoPath) && !logoPath.isEmpty()
                && Objects.nonNull(logoContentId) && !logoContentId.isEmpty()) {
            Resource resource = new ClassPathResource(logoPath);
            helper.addInline(logoContentId, resource);
        }
        mailSender.send(message);
    }

    @Override
    public void sendResourceMail(String to, String subject, String content, String rscPath, String rscId, String... cc) throws MessagingException {
        MimeMessage message = mailSender.createMimeMessage();

        MimeMessageHelper helper = new MimeMessageHelper(message, true);
        helper.setFrom(from);
        helper.setTo(to);
        helper.setSubject(subject);
        helper.setText(content, true);
        if (!ArrayUtils.isEmpty(cc)) {
            helper.setCc(cc);
        }
        FileSystemResource res = new FileSystemResource(new File(rscPath));
        helper.addInline(rscId, res);

        mailSender.send(message);
    }
}
