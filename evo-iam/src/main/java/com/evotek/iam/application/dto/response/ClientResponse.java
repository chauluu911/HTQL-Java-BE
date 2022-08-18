package com.evotek.iam.application.dto.response;

import com.evotek.iam.infrastructure.support.enums.ClientStatus;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

@JsonInclude(JsonInclude.Include.NON_NULL)
@Data
public class ClientResponse {
    private String id;

    private String name;

    private ClientStatus status;

    private Boolean deleted;

    private String roleId;

    private String secretToken;

}
