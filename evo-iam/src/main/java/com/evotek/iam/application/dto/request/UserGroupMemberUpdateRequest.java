package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class UserGroupMemberUpdateRequest extends Request {
    @NotEmpty(message = "USER_MEMBER_IDS_REQUIRED")
    private List<@NotBlank String> userMemberIds;
}
