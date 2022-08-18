package com.evotek.common;

import com.evotek.common.enums.AccountType;
import com.evotek.common.enums.UserLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserAuthority {
    private Instant lastAuthChangeAt;
    private String userId;
    private Boolean isRoot;
    private UserLevel userLevel;
    private AccountType accountType;
    private List<String> grantedPermissions;
}
