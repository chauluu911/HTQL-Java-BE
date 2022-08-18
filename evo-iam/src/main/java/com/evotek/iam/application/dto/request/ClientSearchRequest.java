package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import com.evotek.iam.infrastructure.support.enums.ClientStatus;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ClientSearchRequest extends PagingRequest {
    private String keyword;
    private ClientStatus status;
    private List<String> roleIds;
}
