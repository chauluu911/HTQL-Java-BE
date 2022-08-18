package com.evotek.common.dto.request;

import com.evotek.common.enums.UserLevel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class SearchUserRequest extends PagingRequest {

    private String keyword;

    private List<UserLevel> userLevels;

    public SearchUserRequest(@Min(value = 1, message = "Page index must be greater than 0")
                             @Max(value = 100000, message = "Page index be less than 100000")
                                     int pageIndex,
                             @Min(value = 1, message = "Page size must be greater than 0")
                             @Max(value = 1000, message = "Page size must be less than or equal to 1000")
                                     int pageSize,
                             String sortBy,
                             String keyword,
                             List<UserLevel> userLevels) {
        super(pageIndex, pageSize, sortBy);
        this.keyword = keyword;
        this.userLevels = userLevels;
    }
}
