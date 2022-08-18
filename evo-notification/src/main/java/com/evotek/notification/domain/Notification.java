package com.evotek.notification.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.dto.response.storage.FileDTO;
import com.evotek.common.enums.ContentType;
import com.evotek.common.util.IdUtils;
import com.evotek.notification.infrastructure.support.enums.EventStatus;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.springframework.util.CollectionUtils;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class Notification extends AuditableDomain {
    private String id;
    private String userId;
    private String eventId;
    private Boolean isRead;
    private Instant readAt;
    private Boolean isSend;
    private Instant sendAt;
    private Boolean deleted;

    private String description;
    private String title;
    private EventStatus status;
    private String content;
    private ContentType contentType;
    private ArrayList<AttachFile> attachFiles;

    public Notification(String userId, String eventId) {
        this.id = IdUtils.nextId();
        this.userId = userId;
        this.eventId = eventId;
        this.isRead = false;
        this.isSend = false;
        this.deleted = false;
    }

    public void read() {
        this.isRead = true;
        this.readAt = Instant.now();
    }

    public void unread() {
        this.isRead = false;
        this.readAt = null;
    }

    public void sent() {
        this.isSend = true;
        this.sendAt = Instant.now();
    }

    public void deleted() {
        this.deleted = true;
    }

    public void enrichEvent(Event event) {
        this.description = event.getDescription();
        this.title = event.getTitle();
        this.status = event.getStatus();
        this.content = event.getContent();
        this.contentType = event.getContentType();
    }

    public void enrichAttachFiles(List<FileDTO> files) {
        this.attachFiles = new ArrayList<>();
        if (!CollectionUtils.isEmpty(files)) {
            files.forEach(file -> attachFiles.add(new AttachFile(file.getOriginalName(), file.getViewUrl(), file.getDownloadUrl())));
        }
    }
}
