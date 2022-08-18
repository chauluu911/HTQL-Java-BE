package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.Request;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotEmpty;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class OrganizationGetByIdsRequest extends Request {

    @NotEmpty(message = "IDS_REQUIRED")
    private List<String> ids;
}

