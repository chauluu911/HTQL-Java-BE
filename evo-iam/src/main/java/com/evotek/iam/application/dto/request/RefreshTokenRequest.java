package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotBlank;

@EqualsAndHashCode(callSuper = true)
@Data
public class RefreshTokenRequest extends Request {

    @NotBlank(message = "REFRESH_TOKEN_REQUIRED")
    private String refreshToken;
}
