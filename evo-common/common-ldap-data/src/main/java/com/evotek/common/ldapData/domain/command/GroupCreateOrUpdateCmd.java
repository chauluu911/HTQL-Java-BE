package com.evotek.common.ldapData.domain.command;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class GroupCreateOrUpdateCmd {
    private String name;
    private List<String> members;
    private String description;
}
