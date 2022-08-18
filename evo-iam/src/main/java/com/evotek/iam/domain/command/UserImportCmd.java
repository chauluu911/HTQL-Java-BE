package com.evotek.iam.domain.command;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;
import lombok.experimental.SuperBuilder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class UserImportCmd {
    private String fullName;
    private String email;
    private String phoneNumber;
    private String description;
}
