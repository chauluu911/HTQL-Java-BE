package com.evotek.notification.application.service.impl;

import com.evotek.common.util.StrUtils;
import com.evotek.common.util.StringPool;
import com.evotek.notification.application.mapper.UserTelegramEntityMapper;
import com.evotek.notification.application.service.TelegramBotService;
import com.evotek.notification.domain.UserTelegram;
import com.evotek.notification.infrastructure.persistence.entity.UserTelegramEntity;
import com.evotek.notification.infrastructure.persistence.repository.UserTelegramRepository;
import com.evotek.notification.infrastructure.support.util.Commands;
import com.evotek.notification.infrastructure.support.util.Const;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.telegram.telegrambots.bots.TelegramLongPollingBot;
import org.telegram.telegrambots.meta.TelegramBotsApi;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.InlineKeyboardMarkup;
import org.telegram.telegrambots.meta.api.objects.replykeyboard.buttons.InlineKeyboardButton;
import org.telegram.telegrambots.meta.exceptions.TelegramApiException;
import org.telegram.telegrambots.meta.generics.BotSession;
import org.telegram.telegrambots.updatesreceivers.DefaultBotSession;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;


@Slf4j
@Service
//@EnableScheduling
@RequiredArgsConstructor
public class TelegramBotServiceImpl extends TelegramLongPollingBot implements TelegramBotService {

    private final UserTelegramRepository userTelegramRepository;
    private final UserTelegramEntityMapper userTelegramEntityMapper;
    private BotSession botSession;

    @Value("${bot.token}")
    private String token;

    @Value("${bot.username}")
    private String username;

    @Value("${app.job.delay-stop-polling-telegram-bot}")
    private Long delayStopPolling;

    @PostConstruct
    public void start() {
        log.info("[Telegram Bot] username: {}, token: {}", username, token);
        /**
        try {
            // Make sure that only one bot instance is running
            // Instantiate Telegram Bots API
            TelegramBotsApi botsApi = new TelegramBotsApi(DefaultBotSession.class);
            botSession = botsApi.registerBot(this);

            //stopTelegramBot();
            //startTelegramBot();
        } catch (TelegramApiException e) {
            log.error("[Telegram Bot] Register failed: {0}", e);
        }
         **/
    }

    @Override
    public String getBotUsername() {
        return this.username;
    }

    @Override
    public String getBotToken() {
        return this.token;
    }

    @Override
    @Transactional
    public void onUpdateReceived(Update update) {
        // We check if the update has a message and the message has text
        if (update.hasMessage() && update.getMessage().hasText()) {
            String input = update.getMessage().getText();
            String chatId = String.valueOf(update.getMessage().getChatId());
            if (input.startsWith(Commands.STARTCOMMAND)) {
                String userId = StrUtils.removePrefix(input, Commands.STARTCOMMAND).trim();
                log.info("UserId: {}", userId);
                if (!StrUtils.isBlank(userId)) {
                    String lastName = update.getMessage().getFrom().getLastName();
                    sendMessage(chatId, "Xin chào, " + lastName + " Tôi là bot evotek");
                    handeStartCommand(userId, chatId);
                }
            } else {
                //default message
                SendMessage message = new SendMessage();
                message.setChatId(chatId);
                message.setText(input);
                message.setReplyMarkup(getKeyboard());
                try {
                    execute(message);
                    log.info(Const.TELEGRAM_SEND_MESSAGE, input, chatId);
                } catch (TelegramApiException e) {
                    log.error(Const.TELEGRAM_FAILED_SEND_MESSAGE, input, chatId, e.getMessage());
                }
            }
        } else if (update.hasCallbackQuery()) {
            String input = update.getCallbackQuery().getData();
            String chatId = String.valueOf(update.getCallbackQuery().getMessage().getChatId());
            SendMessage message = new SendMessage();
            message.setChatId(chatId);
            if (input.startsWith(Commands.STARTCOMMAND)) {
                message.setText("Vui lòng nhập mã nhân viên");
            }
            if (input.startsWith(Commands.INFORCOMMAND)) {
                message.setText("Đây là kênh thông báo Evotek");
            } else if (input.startsWith(Commands.HELPCOMMAND)) {
                message.setText("Vui lòng liên hệ với quản trị viên để được hỗ trợ");
            } else if (input.startsWith(Commands.CANCELCOMMAND)) {
                message.setText("Tính năng này chưa được hỗ trợ");
            }
            message.setReplyMarkup(getKeyboard());
            try {
                execute(message);
                log.info(Const.TELEGRAM_SEND_MESSAGE, input, chatId);
            } catch (TelegramApiException e) {
                log.error(Const.TELEGRAM_FAILED_SEND_MESSAGE, input, chatId, e.getMessage());
            }
        }
    }

    @Override
    public void sendMessage(String chatId, String message) {
        final SendMessage request = new SendMessage();
        request.setChatId(chatId);
        request.setText(message);

        try {
            execute(request);
            log.info(Const.TELEGRAM_SEND_MESSAGE, message, chatId);
        } catch (TelegramApiException e) {
            log.error(Const.TELEGRAM_FAILED_SEND_MESSAGE, message, chatId, e.getMessage());
        }
    }

    @Transactional
    public void handeStartCommand(String userId, String chatId) {
        //TODO check userId existed
        Optional<UserTelegramEntity> registerTelegramOpt = userTelegramRepository.findByUserId(userId);
        if (registerTelegramOpt.isPresent()) {
            UserTelegram userTelegram = userTelegramEntityMapper.toDomain(registerTelegramOpt.get());
            userTelegram.updateChatId(chatId);
            saveUserTelegram(userTelegram);
            return;
        }
        UserTelegram userTelegram = new UserTelegram(userId, chatId);
        saveUserTelegram(userTelegram);
    }

    @Transactional
    public void saveUserTelegram(UserTelegram userTelegram) {
        UserTelegramEntity entity = userTelegramEntityMapper.toEntity(userTelegram);
        userTelegramRepository.save(entity);
    }

    //@Scheduled(cron = "${app.job.stop-polling-telegram-bot}")
    public void stopTelegramBot() {
        try {
            if (this.isTelegramBotRunning()) {
                log.info("Stop telegram bot");
                botSession.stop();
            }
        } catch (Exception e) {
            log.error("Catch exception", e);
        }
    }

    @Override
    public void startTelegramBot() {
        try {
            if (Objects.isNull(botSession)) {
                log.info("[Telegram Bot] Register");
                TelegramBotsApi botsApi = new TelegramBotsApi(DefaultBotSession.class);
                botSession = botsApi.registerBot(this);
            }
            if (botSession != null && !botSession.isRunning()) {
                log.info("Start telegram bot");
                botSession.start();
            }
            new java.util.Timer().schedule(
                    new java.util.TimerTask() {
                        @Override
                        public void run() {
                            log.info("Job stop telegram bot");
                            stopTelegramBot();
                        }
                    },
                    delayStopPolling
            );
        } catch (Exception e) { log.error("Catch exception", e); }
    }

    public boolean isTelegramBotRunning() {
        return botSession != null && botSession.isRunning();
    }

    private InlineKeyboardMarkup getKeyboard() {
        InlineKeyboardMarkup inlineKeyboardMarkup = new InlineKeyboardMarkup();

        InlineKeyboardButton start = new InlineKeyboardButton();
        start.setText("Đăng ký nhận thông báo");
        start.setCallbackData(Commands.STARTCOMMAND);

        InlineKeyboardButton info = new InlineKeyboardButton();
        info.setText("Thông tin");
        info.setCallbackData(Commands.INFORCOMMAND);

        InlineKeyboardButton help = new InlineKeyboardButton();
        help.setText("Giúp đỡ");
        help.setCallbackData(Commands.HELPCOMMAND);

        InlineKeyboardButton cancel = new InlineKeyboardButton();
        cancel.setText("Hủy nhận thông báo");
        cancel.setCallbackData(Commands.CANCELCOMMAND);

        List<List<InlineKeyboardButton>> keyboardButtons = new ArrayList<>();

        List<InlineKeyboardButton> keyboardButtonsRow1 = new ArrayList<>();
        keyboardButtonsRow1.add(start);

        List<InlineKeyboardButton> keyboardButtonsRow2 = new ArrayList<>();
        keyboardButtonsRow2.add(info);
        keyboardButtonsRow2.add(help);

        List<InlineKeyboardButton> keyboardButtonsRow3 = new ArrayList<>();
        keyboardButtonsRow3.add(cancel);

        keyboardButtons.add(keyboardButtonsRow1);
        keyboardButtons.add(keyboardButtonsRow2);
        keyboardButtons.add(keyboardButtonsRow3);

        inlineKeyboardMarkup.setKeyboard(keyboardButtons);

        return inlineKeyboardMarkup;
    }

    @Override
    public void senMessageConfirmMeetingParticipation(String chatId, String message, String meetingId) {
        final SendMessage request = new SendMessage();
        request.setChatId(chatId);
        request.setText(message);
        request.setReplyMarkup(getKeyboardConfirmMeetingParticipation(meetingId));
        try {
            execute(request);
            log.info(Const.TELEGRAM_SEND_MESSAGE, message, chatId);
        } catch (TelegramApiException e) {
            log.error(Const.TELEGRAM_FAILED_SEND_MESSAGE, message, chatId, e.getMessage());
        }
    }

    private InlineKeyboardMarkup getKeyboardConfirmMeetingParticipation(String meetingId) {
        InlineKeyboardMarkup inlineKeyboardMarkup = new InlineKeyboardMarkup();

        InlineKeyboardButton participate = new InlineKeyboardButton();
        participate.setText("Tham gia");
        participate.setCallbackData(Commands.PARTICIPATEMEETINGCOMMAND + StringPool.SPACE + meetingId);

        InlineKeyboardButton reject = new InlineKeyboardButton();
        reject.setText("Từ chối");
        reject.setCallbackData(Commands.REJECTMEETINGCOMMAND + StringPool.SPACE + meetingId);

        List<List<InlineKeyboardButton>> keyboardButtons = new ArrayList<>();

        List<InlineKeyboardButton> keyboardButtonsRow1 = new ArrayList<>();
        keyboardButtonsRow1.add(participate);
        keyboardButtonsRow1.add(reject);

        keyboardButtons.add(keyboardButtonsRow1);
        inlineKeyboardMarkup.setKeyboard(keyboardButtons);

        return inlineKeyboardMarkup;
    }

}
