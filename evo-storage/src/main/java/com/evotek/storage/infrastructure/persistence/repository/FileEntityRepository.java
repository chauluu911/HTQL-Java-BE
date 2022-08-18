package com.evotek.storage.infrastructure.persistence.repository;

import com.evotek.storage.infrastructure.persistence.entity.FileEntity;
import com.evotek.storage.infrastructure.persistence.repository.custom.FileEntityRepositoryCustom;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface FileEntityRepository extends JpaRepository<FileEntity, String>, FileEntityRepositoryCustom {

    @Override
    @Query("from FileEntity f where f.deleted = false and f.id = :id ")
    Optional<FileEntity> findById(String id);

    @Query("from FileEntity  f where f.deleted = false and f.originalName = :originalName " +
            " and f.ownerId = :ownerId and f.ownerType = :ownerType order by f.version desc ")
    List<FileEntity> findAllByOriginalNameAndAndOwnerIdAndOwnerType(String originalName, String ownerId, String ownerType);

    default Optional<FileEntity> findFirstByOriginalNameAndAndOwnerIdAndOwnerType(String originalName, String ownerId, String ownerType) {
        return findAllByOriginalNameAndAndOwnerIdAndOwnerType(originalName, ownerId, ownerType)
                .stream().findFirst();
    }

    @Query("from FileEntity f where f.deleted = false and f.id in :ids ")
    List<FileEntity> findByIds(List<String> ids);

}
