package com.evotek.iam.infrastructure.persistence.repository;

import com.evotek.iam.infrastructure.persistence.entity.DepartmentEntity;
import com.evotek.iam.infrastructure.persistence.repository.custom.DepartmentRepositoryCustom;
import com.evotek.iam.infrastructure.support.enums.DepartmentStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface DepartmentEntityRepository extends JpaRepository<DepartmentEntity, String>, DepartmentRepositoryCustom {

    @Query("from DepartmentEntity de where de.deleted = false")
    List<DepartmentEntity> findAllActivated();

    @Query("from DepartmentEntity de where de.parentId = :parentId and de.deleted = false ")
    List<DepartmentEntity> findAllByParentActivated(String parentId);

    @Query("from DepartmentEntity de where de.code = :code and de.deleted = false ")
    Optional<DepartmentEntity> findByCode(@Param("code") String code);

    @Query("from DepartmentEntity de where de.id = :parentId and de.deleted = false ")
    Optional<DepartmentEntity> findByDepartmentId(String parentId);

    @Query("from DepartmentEntity de where (de.parentPath is null or de.parentPath not like CONCAT('%',:id,'%')) and de.id <> :id  and de.deleted = false ")
    List<DepartmentEntity> findAllExceptActivated(@Param("id") String id);

    @Query("from DepartmentEntity e where e.parentId is null and e.id <> :id and e.deleted = false ")
    List<DepartmentEntity> findAllRootExcept(@Param("id") String id);

    @Query("from DepartmentEntity de where de.id in :ids and de.deleted = false ")
    List<DepartmentEntity> findAllByIds(List<String> ids);

    @Query("from DepartmentEntity de where de.deleted = false and " +
            " (:keyword is null or de.name like :keyword or de.code like :keyword) " +
            " AND (:status is null OR de.status = :status) " +
            " OR (COALESCE(:departmentIds, null) IS NULL OR de.id IN (:departmentIds)) ")
    Page<DepartmentEntity> searchAutoComplete(String keyword,
                                              DepartmentStatus status,
                                              List<String> departmentIds,
                                              Pageable pageable);

    @Query("from DepartmentEntity de where de.parentId is null and  de.deleted = false ")
    List<DepartmentEntity> findAllRoots();

    @Query("from DepartmentEntity de where de.name = :name  and  de.deleted = false ")
    DepartmentEntity findDepartmentByDepartmentName(String name);
}
