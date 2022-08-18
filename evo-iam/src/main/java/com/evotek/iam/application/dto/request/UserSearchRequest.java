package com.evotek.iam.application.dto.request;

import com.evotek.common.dto.request.PagingRequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class UserSearchRequest extends PagingRequest {
    private String keyword;
    private String status;
    private String accountType;
    private List<String> roleIds;
    private List<String> groupIds;
    private boolean searchByGroup;

}

