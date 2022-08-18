package com.evotek.common.dto.request;

import com.evotek.common.validator.ValidateUUID;
import lombok.*;
import lombok.extern.slf4j.Slf4j;

import javax.validation.constraints.NotEmpty;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Slf4j
public class FindByIdsRequest extends Request {

    @NotEmpty(message = "IDS_REQUIRED")
    private List<@ValidateUUID String> ids;
}
