package com.evotek.iam.infrastructure.persistence.repository;

import com.evotek.iam.infrastructure.persistence.entity.UserGroupEntity;
import com.evotek.iam.infrastructure.persistence.repository.custom.GroupRepositoryCustom;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface UserGroupEntityRepository extends JpaRepository<UserGroupEntity, String>,GroupRepositoryCustom{
    @Query("from UserGroupEntity g where g.id = :id and g.deleted = false")
    UserGroupEntity findByGroupId(@Param("id") String id);
}
