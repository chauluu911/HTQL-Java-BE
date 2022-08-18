package com.evotek.iam.infrastructure.persistence.repository;

import com.evotek.iam.infrastructure.persistence.entity.ClientEntity;
import com.evotek.iam.infrastructure.persistence.repository.custom.ClientRepositoryCustom;
import com.evotek.iam.infrastructure.support.enums.ClientStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface ClientEntityRepository extends JpaRepository<ClientEntity, String>, ClientRepositoryCustom {

    @Query("from ClientEntity ce where ce.deleted = false and ce.id = :clientId and ce.secret = :clientSecret" +
            " and ce.status = :status")
    Optional<ClientEntity> findByClientIdAndSecret(@Param("clientId") String clientId,
                                                   @Param("clientSecret") String clientSecret,
                                                   @Param("status") ClientStatus status);

    @Query("from ClientEntity ce where ce.deleted = false and lower(ce.name) = :clientName")
    Optional<ClientEntity> findByClientName(@Param("clientName") String clientName);
}
