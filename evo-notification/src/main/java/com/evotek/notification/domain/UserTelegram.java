package com.evotek.notification.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import lombok.*;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class UserTelegram extends AuditableDomain {
    private String id;
    private String userId;
    private String chatId;
    private Boolean deleted;

    public UserTelegram(String userId, String chatId) {
        this.id = IdUtils.nextId();
        this.userId = userId;
        this.chatId = chatId;
        this.deleted = false;
    }

    public void updateChatId(String chatId) {
        this.chatId = chatId;
    }

    public void delete() {
        this.deleted = true;
    }
}
