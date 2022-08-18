package com.evotek.iam.domain.command;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class UserCustomerCreateCmd extends UserCreateCmd {
    private String password;
    private String repeatPassword;
    private List<String> roleIds;
}
