package com.evotek.common.dto.response.storage;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class FileDTO implements Serializable {

    private String id;
    private String path;
    private String originalName;
    private String hashed;
    private String downloadUrl;
    private String viewUrl;

    private Long size;
    private String type;
    private String ownerId;
    private String ownerType;
    private Boolean deleted;
    private Long version;

}
