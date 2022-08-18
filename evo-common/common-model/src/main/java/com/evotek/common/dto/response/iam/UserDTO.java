package com.evotek.common.dto.response.iam;

import com.evotek.common.enums.AccountType;
import com.evotek.common.enums.AuthenticationType;
import com.evotek.common.enums.UserLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserDTO implements Serializable {
    private String id;
    private String username;
    private String fullName;
    private String email;
    private String phoneNumber;
    private String employeeCode;
    private String departmentName;
    private String title;
    private AuthenticationType authenticationType;

    private AccountType accountType;
    private UserLevel userLevel;
}
