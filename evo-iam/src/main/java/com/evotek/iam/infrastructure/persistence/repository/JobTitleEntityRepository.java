package com.evotek.iam.infrastructure.persistence.repository;

import com.evotek.iam.infrastructure.persistence.entity.JobTitleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import javax.swing.text.html.Option;
import java.util.List;
import java.util.Optional;

public interface JobTitleEntityRepository extends JpaRepository<JobTitleEntity, String> {

    @Query("from JobTitleEntity jte where jte.deleted = false and jte.status = 'ACTIVE' and jte.id = :id")
    Optional<JobTitleEntity> findJobTitleActiveById(@Param("id") String id);

    @Query("from JobTitleEntity jte where jte.deleted = false and jte.code = :code")
    Optional<JobTitleEntity> findByJobTitleCode(@Param("code") String code);

    @Query("from JobTitleEntity jte where jte.deleted = false and lower(jte.status) = 'active'")
    List<JobTitleEntity> findAllJobTitle();

    @Query("from JobTitleEntity jte where jte.deleted = false and jte.isManager = :isManager")
    List<JobTitleEntity> findJobTitleIsManager(@Param("isManager") Boolean isManager);

}
