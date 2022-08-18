package com.evotek.iam.domain.command;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserGroupUpdateCmd {
    private String name;

    private String description;

    List<String> userMemberIds;
}