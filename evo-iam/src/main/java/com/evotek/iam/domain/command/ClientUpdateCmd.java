package com.evotek.iam.domain.command;

import com.evotek.iam.infrastructure.support.enums.ClientStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ClientUpdateCmd {
    private String name;
    private ClientStatus status;
    private String roleId;
}
