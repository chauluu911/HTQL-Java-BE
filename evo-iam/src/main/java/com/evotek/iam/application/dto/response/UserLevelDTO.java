package com.evotek.iam.application.dto.response;

import lombok.*;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Setter(AccessLevel.PRIVATE)
@Getter
public class UserLevelDTO {
    private String userLevelKey;
    private String userLevelValue;
}
