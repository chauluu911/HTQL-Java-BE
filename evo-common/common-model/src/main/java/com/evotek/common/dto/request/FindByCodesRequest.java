package com.evotek.common.dto.request;

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
public class FindByCodesRequest extends Request {
    @NotEmpty(message = "CODE_REQUIRED")
    private List<String> codes;
}
