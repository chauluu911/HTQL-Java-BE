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
public class UserGroupMemberUpdateCmd {
    private List<String> userMemberIds;
}
