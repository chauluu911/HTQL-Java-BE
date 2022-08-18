package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import lombok.*;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LogoutRequest extends Request {

    private String deviceId;

    private String deviceToken;

    private String refreshToken;
}
