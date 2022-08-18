package com.evotek.iam.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.IdUtils;
import com.evotek.iam.domain.command.UserGroupMemberUpdateCmd;
import com.evotek.iam.domain.command.UserGroupCreateCmd;
import com.evotek.iam.domain.command.UserGroupUpdateCmd;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.*;
import lombok.experimental.SuperBuilder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@EqualsAndHashCode(callSuper = false)
@Data
@SuperBuilder
@NoArgsConstructor
@Setter(AccessLevel.PRIVATE)
@Getter
@Slf4j
public class UserGroup extends AuditableDomain {
    private String id;
    private String name;
    private String description;
    private String code;
    private Boolean deleted;
    private List<User> members;
    private Integer totalMember;
    @JsonIgnore
    private List<UserGroupMember> userGroupMembers;

    public UserGroup(UserGroupCreateCmd cmd, List<User> existedUser) {
        this.id = IdUtils.nextId();
        this.name = cmd.getName();
        this.description = cmd.getDescription();
        this.code = cmd.getCode();
        this.deleted = false;
        this.userGroupMembers = new ArrayList<>();
        List<String> assignedUserMemberIds;
        if (CollectionUtils.isEmpty(cmd.getUserMemberIds())) {
            assignedUserMemberIds = new ArrayList<>();
        } else {
            assignedUserMemberIds = cmd.getUserMemberIds().stream().distinct().collect(Collectors.toList());
        }
        assignedUserMemberIds.forEach(assignedId -> {
            Optional<User> userMemberOptional = existedUser.stream().filter(item -> item.getId().equals(assignedId)).findFirst();
            if (userMemberOptional.isEmpty()) {
                throw new ResponseException(BadRequestError.USER_MEMBER_INVALID);
            } else {
                this.userGroupMembers.add(new UserGroupMember(this.id, assignedId));
            }
        });
        this.totalMember = this.userGroupMembers.size();
    }

    public void update(UserGroupUpdateCmd cmd, List<User> existedUser) {
        this.name = cmd.getName();
        this.description = cmd.getDescription();
        this.members = this.members == null ? new ArrayList<>() : this.members;
        if (CollectionUtils.isEmpty(this.userGroupMembers)) {
            this.userGroupMembers = new ArrayList<>();
        } else {
            this.userGroupMembers.forEach(UserGroupMember::deleted);
            this.totalMember = 0;
        }
        if (CollectionUtils.isEmpty(cmd.getUserMemberIds())) {
            this.userGroupMembers = new ArrayList<>();
        }
        List<String> assignedUserMemberIds;
        if (CollectionUtils.isEmpty(cmd.getUserMemberIds())) {
            assignedUserMemberIds = new ArrayList<>();
        } else {
            assignedUserMemberIds = cmd.getUserMemberIds().stream().distinct().collect(Collectors.toList());
        }
        assignedUserMemberIds.forEach(assignedId -> {
            Optional<User> userMemberOptional = existedUser.stream().filter(item -> item.getId().equals(assignedId)).findFirst();
            if (userMemberOptional.isEmpty()) {
                throw new ResponseException(BadRequestError.USER_MEMBER_INVALID);
            } else {
                Optional<UserGroupMember> userGroupMemberOptional = this.userGroupMembers.stream().filter(u -> u.getUserId().equals(assignedId)).findFirst();
                if (userGroupMemberOptional.isEmpty()) {
                    this.userGroupMembers.add(new UserGroupMember(this.id, assignedId));
                } else {
                    userGroupMemberOptional.get().unDelete();
                }
                this.totalMember++;
            }
        });
        this.deleted = false;
    }

    public void removeListMemberFromGroup(UserGroupMemberUpdateCmd cmd, List<User> existedUser) {
        List<String> assignedUserMemberIds;
        if (CollectionUtils.isEmpty(cmd.getUserMemberIds())) {
            assignedUserMemberIds = new ArrayList<>();
        } else {
            assignedUserMemberIds = cmd.getUserMemberIds().stream().distinct().collect(Collectors.toList());
        }
        assignedUserMemberIds.forEach(assignedId -> {
            Optional<User> userMemberOptional = existedUser.stream().filter(item -> item.getId().equals(assignedId)).findFirst();
            if (userMemberOptional.isEmpty()) {
                throw new ResponseException(BadRequestError.USER_MEMBER_INVALID);
            } else {
                Optional<UserGroupMember> userGroupMemberOptional = this.userGroupMembers.stream().filter(item -> item.getUserId().equals(assignedId)).findFirst();
                if (userGroupMemberOptional.isEmpty()) {
                    throw new ResponseException(BadRequestError.USER_IS_NOT_IN_GROUP);
                } else {
                    userGroupMemberOptional.get().deleted();
                }
                this.totalMember = this.totalMember > 0 ? this.totalMember - 1 : 0;
            }
        });
    }

    public void addListMemberToGroup(UserGroupMemberUpdateCmd cmd, List<User> existedUser) {
        List<String> assignedUserMemberIds;
        if (CollectionUtils.isEmpty(cmd.getUserMemberIds())) {
            assignedUserMemberIds = new ArrayList<>();
        } else {
            assignedUserMemberIds = cmd.getUserMemberIds().stream().distinct().collect(Collectors.toList());
        }
        assignedUserMemberIds.forEach(assignedId -> {
            Optional<User> userMemberOptional = existedUser.stream().filter(item -> item.getId().equals(assignedId)).findFirst();
            if (userMemberOptional.isEmpty()) {
                throw new ResponseException(BadRequestError.USER_MEMBER_INVALID);
            } else {
                Optional<UserGroupMember> userGroupMemberOptional = this.userGroupMembers.stream().filter(item -> item.getUserId().equals(assignedId)).findFirst();
                if (userGroupMemberOptional.isPresent()) {
                    throw new ResponseException(BadRequestError.USER_IS_ALREADY_IN_GROUP);
                } else {
                    this.userGroupMembers.add(new UserGroupMember(this.id, assignedId));
                }
                this.totalMember++;
            }
        });
    }

    public void deleted() {
        this.deleted = true;
        if (!CollectionUtils.isEmpty(this.userGroupMembers)) {
            this.userGroupMembers.forEach(UserGroupMember::deleted);
        }
        this.totalMember = 0;
    }

    public void enrichMembers(List<User> members) {
        this.members = members;
    }

    public void enrichUserGroupMembers(List<UserGroupMember> userGroupMembers) {
        this.userGroupMembers = userGroupMembers;
    }

}
