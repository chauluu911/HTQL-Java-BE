package com.evotek.iam.domain.command;

import com.evotek.iam.infrastructure.support.enums.AuthenticationType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class UserInternalCreateCmd extends UserCreateCmd {
    private String password;
    private AuthenticationType authenticationType;
    private List<String> roleIds;
    private String employeeCode;
    private String title;
}
