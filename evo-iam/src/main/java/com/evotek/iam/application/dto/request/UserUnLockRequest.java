package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotEmpty;
import java.util.ArrayList;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class UserUnLockRequest extends Request {

    @NotEmpty(message = "USERNAME_REQUIRED")
    private List<String> usernames = new ArrayList<>();
}
