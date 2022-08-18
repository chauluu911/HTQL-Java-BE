package com.evotek.iam.infrastructure.persistence.repository;

import com.evotek.iam.infrastructure.persistence.entity.EmploymentHistoryEntity;
import com.evotek.iam.infrastructure.support.enums.EmploymentStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface EmploymentHistoryEntityRepository extends JpaRepository<EmploymentHistoryEntity, String> {
    @Query("from EmploymentHistoryEntity de where de.userId = :userId and de.deleted = false and de.status = :status ")
    EmploymentHistoryEntity findEmploymentHistoryActiveByUserId(@Param("userId") String userId, @Param("status") EmploymentStatus status);

    @Query("from EmploymentHistoryEntity de where de.userId = :userId and de.deleted = false")
    List<EmploymentHistoryEntity> findAllEmploymentHistoriesByUserId(@Param("userId") String userId);

    @Query("from EmploymentHistoryEntity ehe where ehe.departmentId = :departmentId and ehe.jobTitleId in :jobTitleIds and ehe.status = :status ")
    List<EmploymentHistoryEntity> findEmploymentHistoryActiveManager(@Param("departmentId") String departmentId,@Param("jobTitleIds") List<String> jobTitleIds, @Param("status") EmploymentStatus status);

    @Query(value = "SELECT * From employment_history where job_title_id = :jobTitleId limit 1", nativeQuery = true)
    Optional<EmploymentHistoryEntity> findEmploymentHistoryByJobTitleId(@Param("jobTitleId") String jobTitleId);
}