package com.evotek.notification.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import com.evotek.notification.infrastructure.support.enums.EventFileType;
import lombok.*;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class EventFile extends AuditableDomain {

    private String id;
    private String fileId;
    private String eventId;
    private EventFileType type;
    private Boolean deleted;

    private String originalName;
    private String viewUrl;
    private String downloadUrl;

    public EventFile(String fileId, String eventId, EventFileType type) {
        this.id = IdUtils.nextId();
        this.fileId = fileId;
        this.eventId = eventId;
        this.type = type;
        this.deleted = false;
    }

    public void enrichFile(String originalName, String viewUrl, String downloadUrl) {
        this.originalName = originalName;
        this.viewUrl = viewUrl;
        this.downloadUrl = downloadUrl;
    }

    public void delete() {
        this.deleted = true;
    }

    public void unDelete() {
        this.deleted = false;
    }
}
