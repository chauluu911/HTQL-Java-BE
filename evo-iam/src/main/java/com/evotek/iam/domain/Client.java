package com.evotek.iam.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import com.evotek.common.util.RandomUtils;
import com.evotek.iam.domain.command.ClientCreateCmd;
import com.evotek.iam.domain.command.ClientUpdateCmd;
import com.evotek.iam.infrastructure.support.enums.ClientStatus;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.*;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class Client extends AuditableDomain {

    private String id;

    private String name;

    @JsonIgnore
    private String secret;

    private ClientStatus status;

    private Boolean deleted;

    private String roleId;

    private Role role;

    public Client(ClientCreateCmd cmd) {
        this.id = IdUtils.nextId();
        this.name = cmd.getName();
        this.roleId = cmd.getRoleId();
        this.secret = RandomUtils.generateSecret();
        this.status = ClientStatus.ACTIVE;
        this.deleted = false;
    }

    public void update(ClientUpdateCmd cmd) {
        this.name = cmd.getName();
        this.status = cmd.getStatus();
        this.roleId = cmd.getRoleId();

        this.deleted = false;
    }

    public void changeStatus(ClientStatus status) {
        this.status = status;
    }

    public void delete() {
        this.deleted = true;
    }

    public void enrichRole(Role role) {
        this.role = role;
    }
}
