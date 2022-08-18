package com.evotek.iam.infrastructure.persistence.repository;

import com.evotek.iam.infrastructure.persistence.entity.RolePermissionEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface RolePermissionEntityRepository extends JpaRepository<RolePermissionEntity, String> {

    @Query("from RolePermissionEntity u where u.deleted = false and u.roleId = :roleId")
    List<RolePermissionEntity> findAllByRoleId(@Param("roleId") String roleId);

    @Query("from RolePermissionEntity u where u.deleted = false and u.roleId in :roleIds")
    List<RolePermissionEntity> findAllByRoleIds(@Param("roleIds") List<String> roleIds);
}
