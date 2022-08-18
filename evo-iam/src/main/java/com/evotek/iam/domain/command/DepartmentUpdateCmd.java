package com.evotek.iam.domain.command;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DepartmentUpdateCmd {
    private String name;
    private String parentId;
    private String description;
}
