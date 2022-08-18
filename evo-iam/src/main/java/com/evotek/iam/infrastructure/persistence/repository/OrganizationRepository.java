package com.evotek.iam.infrastructure.persistence.repository;

import com.evotek.iam.infrastructure.persistence.entity.OrganizationEntity;
import com.evotek.iam.infrastructure.persistence.repository.custom.OrganizationRepositoryCustom;
import com.evotek.iam.infrastructure.support.enums.OrganizationStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Collections;
import java.util.List;
import java.util.Optional;


@Repository
public interface OrganizationRepository extends JpaRepository<OrganizationEntity, String>, OrganizationRepositoryCustom {

    @Query("from OrganizationEntity u where u.deleted = false and u.id in :ids " +
            " order by u.name asc")
    List<OrganizationEntity> findAllByIds(@Param("id") List<String> ids);

    default Optional<OrganizationEntity> findByIds( String ids){
        return findAllByIds(Collections.singletonList(ids)).stream().findFirst();
    }

    @Query("select f from OrganizationEntity f where f.deleted = false" +
            " and (:keyword is null or (" +
            " f.name like :keyword" +
            " ))")
    Page<OrganizationEntity> search(@Param("keyword") String keyword, Pageable pageable);

    @Query("select f from OrganizationEntity f where f.deleted = false and f.status = :status" +
            " and (:keyword is null or (" +
            " f.name like :keyword" +
            " ))")
    Page<OrganizationEntity> search(@Param("keyword") String keyword,@Param("status") OrganizationStatus status, Pageable pageable);

    @Query("from OrganizationEntity u where u.deleted = false and u.businessCode = :businessCode")
    Optional<OrganizationEntity> findByBusinessCode(@Param("businessCode") String businessCode);

    @Query("from OrganizationEntity u where u.deleted = false and u.email = :email")
    Optional<OrganizationEntity> findByEmail(@Param("email") String email);
}

