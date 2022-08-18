package com.evotek.iam.infrastructure.persistence.repository;

import com.evotek.iam.infrastructure.persistence.entity.PermissionEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PermissionEntityRepository extends JpaRepository<PermissionEntity, String> {

    @Query("from PermissionEntity u where u.deleted = false and u.id in :ids")
    List<PermissionEntity> findAllByIds(@Param("ids") List<String> ids);

    @Query("from PermissionEntity u where u.deleted = false order by u.priority, u.createdAt ")
    List<PermissionEntity> findAllActivated();
}
