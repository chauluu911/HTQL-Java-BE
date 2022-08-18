package com.evotek.common.ldapData.domain.command;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UserCreateOrUpdateCmd {
    private String fullName;
    private String userName;
    private String password;
    private List<String> memberOf;
}
