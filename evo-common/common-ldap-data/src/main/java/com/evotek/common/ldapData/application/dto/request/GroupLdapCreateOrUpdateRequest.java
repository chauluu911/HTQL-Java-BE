package com.evotek.common.ldapData.application.dto.request;

import com.evotek.common.dto.request.Request;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class GroupLdapCreateOrUpdateRequest extends Request {
    @NotBlank(message = "NAME_REQUIRED")
    private String name;
    private List<String> members;
    private String description;
}
