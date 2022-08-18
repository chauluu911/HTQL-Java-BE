package com.evotek.iam.infrastructure.persistence.repository;

import com.evotek.iam.infrastructure.persistence.entity.UserGroupMemberEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface UserGroupMemberEntityRepository extends JpaRepository<UserGroupMemberEntity, String> {
    @Query("from UserGroupMemberEntity g where g.groupId in :groupIds and g.deleted = false ")
    List<UserGroupMemberEntity> findAllMemberByGroupIds(@Param("groupIds") List<String> groupIds);

    @Query("from UserGroupMemberEntity g where g.userId in :userIds and g.groupId = :groupId and g.deleted = false ")
    List<UserGroupMemberEntity> findAllMemberInGroup(@Param("userIds") List<String> userIds, @Param("groupId") String groupId);
}
