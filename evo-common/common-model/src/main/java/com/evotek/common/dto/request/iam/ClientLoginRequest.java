package com.evotek.common.dto.request.iam;

import com.evotek.common.dto.request.Request;
import com.evotek.common.validator.ValidateConstraint;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ClientLoginRequest extends Request {

    @NotBlank(message = "CLIENT_ID_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.NAME_MAX_LENGTH, message = "CLIENT_ID_LENGTH")
    private String clientId;

    @NotBlank(message = "CLIENT_SECRET_REQUIRED")
    @Size(max = ValidateConstraint.LENGTH.VALUE_MAX_LENGTH,
            message = "CLIENT_SECRET_LENGTH")
    private String clientSecret;
}
