package com.evotek.storage.domain.command;

import lombok.*;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
public class FileCreateCmd {

    private String path;
    private String originalName;
    private Long size;
    private String type;
    private String ownerId;
    private String ownerType;
    private String hashed;
    private Long version;
}
