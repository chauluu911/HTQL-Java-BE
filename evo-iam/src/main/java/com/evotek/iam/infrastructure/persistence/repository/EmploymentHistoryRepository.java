package com.evotek.iam.infrastructure.persistence.repository;

import com.evotek.iam.infrastructure.persistence.entity.EmploymentHistoryEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface EmploymentHistoryRepository extends JpaRepository<EmploymentHistoryEntity, String> {

    @Query("select e from EmploymentHistoryEntity e where e.userId = :userId and e.deleted = false")
    List<EmploymentHistoryEntity> findAllByUserId(String userId);
}
