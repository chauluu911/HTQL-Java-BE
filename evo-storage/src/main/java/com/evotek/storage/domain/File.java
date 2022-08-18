package com.evotek.storage.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import com.evotek.storage.domain.command.FileCreateCmd;
import com.evotek.storage.infrastructure.support.util.StringUtil;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.springframework.util.StringUtils;

import java.util.Objects;
import java.util.UUID;

@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class File extends AuditableDomain {

    private String id;
    private String path;
    private String originalName;
    private String hashed;
    private Long size;
    private String type;
    private String ownerId;
    private String ownerType;
    private Boolean deleted;
    private Long version;
    private String downloadUrl;
    private String viewUrl;

    @JsonIgnore
    private String publicUrl;

    public File(FileCreateCmd cmd) {
        this.id = IdUtils.nextId();
        this.path = cmd.getPath();
        this.originalName = cmd.getOriginalName();
        this.size = cmd.getSize();
        this.type = cmd.getType();
        this.ownerId = cmd.getOwnerId();
        this.ownerType = cmd.getOwnerType();
        this.hashed = cmd.getHashed();
        this.version = cmd.getVersion();
        this.deleted = false;
    }

    public static String genNewFileName(String originalName, String hashed) {

        if (!StringUtils.hasLength(hashed)) {
            hashed = UUID.randomUUID().toString();
        }
        String newFileName;
        String fullName = StringUtil.getSafeFileName(Objects.requireNonNull(originalName));
        String fileType = "";
        if (StringUtils.hasText(fullName)) {
            int last = fullName.lastIndexOf(".");
            if (last >= 0) {
                fileType = fullName.substring(last);
            }
        }
        newFileName = String.format("%s-%s%s", hashed, UUID.randomUUID().toString().substring(0, 8), fileType);
        return newFileName;
    }

    public String getDownloadUrl() {
        return String.format("%s/%s/download", this.publicUrl, id);
    }

    public String getViewUrl() {
        if (Objects.nonNull(this.type) && (this.type.contains("image") || this.isImage())) {
            return String.format("%s/%s/view", this.publicUrl, id);
        }
        return null;
    }

    public void deleted() {
        this.deleted = true;
    }

    public void enrichPublicUrl(String host) {
        this.publicUrl = host;
    }

    public boolean isImage() {
        return this.type.contains("image") || this.type.contains("img");
    }
}
