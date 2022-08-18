package com.evotek.notification.domain;

import lombok.Data;

import java.io.Serializable;

@Data
public class AttachFile implements Serializable {

    private String originalName;
    private String viewUrl;
    private String downloadUrl;

    public AttachFile(String originalName, String viewUrl, String downloadUrl) {
        this.originalName = originalName;
        this.viewUrl = viewUrl;
        this.downloadUrl = downloadUrl;
    }
}
