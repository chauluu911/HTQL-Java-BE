package com.evotek.iam.infrastructure.persistence.repository;

import com.evotek.iam.infrastructure.persistence.entity.UserEntity;
import com.evotek.iam.infrastructure.persistence.repository.custom.UserRepositoryCustom;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface UserEntityRepository extends JpaRepository<UserEntity, String>, UserRepositoryCustom {

    @Query("from UserEntity u where u.deleted = false and lower(u.username) = lower(:username)")
    List<UserEntity> findAllByUsername(@Param("username") String username);

    default Optional<UserEntity> findByUsername(String username) {
        return findAllByUsername(username).stream().findFirst();
    }

    @Query("from UserEntity u where u.deleted = false and lower(u.employeeCode) = lower(:employeeCode)")
    List<UserEntity> findAllByEmployeeCode(@Param("employeeCode") String employeeCode);

    default Optional<UserEntity> findByEmployeeCode(String employeeCode) {
        return findAllByEmployeeCode(employeeCode).stream().findFirst();
    }

    @Query("from UserEntity u where u.deleted = false and lower(u.email) in :emails")
    List<UserEntity> findAllByEmails(@Param("emails") List<String> emails);

    @Query("from UserEntity u where u.deleted = false and u.phoneNumber = :phoneNumber")
    List<UserEntity> findByAllPhoneNumber(@Param("phoneNumber") String phoneNumber);

    default Optional<UserEntity> findByPhoneNumber(String phoneNumber) {
        return findByAllPhoneNumber(phoneNumber).stream().findFirst();
    }

    @Query("from UserEntity u where u.deleted = false and lower(u.email) = lower(:email)")
    Optional<UserEntity> findByEmail(@Param("email") String email);

    @Query("from UserEntity u where u.deleted = false and u.id in :ids")
    List<UserEntity> findByIds(@Param("ids") List<String> ids);
    @Query("from UserEntity u where u.deleted = false and u.status = :status and " +
            " ( :keyword is null or ( u.username like :keyword or" +
            " u.fullName like :keyword or u.phoneNumber like :keyword or " +
            " u.email like :keyword))")
    Page<UserEntity> search(@Param("keyword") String keyword, @Param("status") UserStatus status, Pageable pageable);

    @Query("from UserEntity u where u.deleted = false ")
    List<UserEntity> findAllUserActive();

    @Query("from UserEntity e where e.departmentId in :departmentIds and e.deleted = false and e.status = :status")
    List<UserEntity> findAllUserByDepartmentIds(List<String> departmentIds, UserStatus status);

    @Query(value = "Select * from users where users.job_title_id = :jobTitleId limit 1", nativeQuery = true)
    Optional<UserEntity> findFirstUserByJobTitleId(@Param("jobTitleId") String jobTitleId);

}
