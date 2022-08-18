package com.evotek.common.ldapcore.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class UserLdapResponse {
    private String userName;
    private String displayName;
    private String userPrincipalName;
    private String mail;
    private String telephoneNumber;
    private String memberOf;
}

