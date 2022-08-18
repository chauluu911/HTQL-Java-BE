package com.evotek.storage.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class FileSearchRequest extends PagingRequest {

    private String keyword;

    private String ownerId;

    private String ownerType;
}
