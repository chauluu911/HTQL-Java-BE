package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
public class RoleAutocompleteRequest extends PagingRequest {

    private String keyword;
}
