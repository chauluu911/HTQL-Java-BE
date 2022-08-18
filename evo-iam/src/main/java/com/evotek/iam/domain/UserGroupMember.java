package com.evotek.iam.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import lombok.*;
import lombok.experimental.SuperBuilder;
import lombok.extern.slf4j.Slf4j;

@EqualsAndHashCode(callSuper = false)
@Data
@SuperBuilder
@NoArgsConstructor
@Setter(AccessLevel.PRIVATE)
@Getter
@Slf4j
public class UserGroupMember extends AuditableDomain {
    private String id;
    private String userId;
    private String groupId;
    private Boolean deleted;

    public UserGroupMember(String groupId, String userId) {
        this.id = IdUtils.nextId();
        this.userId = userId;
        this.groupId = groupId;
        this.deleted = false;
    }

    public void deleted() {
        this.deleted = true;
    }
    public void unDelete() {
        this.deleted = false;
    }

}
