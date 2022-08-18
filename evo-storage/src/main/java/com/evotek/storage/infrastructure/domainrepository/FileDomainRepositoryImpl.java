package com.evotek.storage.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.storage.application.config.StorageProperties;
import com.evotek.storage.domain.File;
import com.evotek.storage.domain.repositoty.FileDomainRepository;
import com.evotek.storage.infrastructure.persistence.entity.FileEntity;
import com.evotek.storage.infrastructure.persistence.mapper.FileEntityMapper;
import com.evotek.storage.infrastructure.persistence.repository.FileEntityRepository;
import com.evotek.storage.infrastructure.support.exception.NotFoundError;
import org.springframework.stereotype.Repository;

@Repository
public class FileDomainRepositoryImpl extends AbstractDomainRepository<File, FileEntity, String> implements FileDomainRepository {
    private final StorageProperties properties;

    protected FileDomainRepositoryImpl(FileEntityRepository jpaRepository, FileEntityMapper mapper, StorageProperties properties) {
        super(jpaRepository, mapper);
        this.properties = properties;
    }

    @Override
    public File getById(String id) {
        File file = this.findById(id).orElseThrow(() -> new ResponseException(NotFoundError.FILE_NOT_FOUND));
        return file;
    }

    @Override
    protected File enrich(File file) {
        file.enrichPublicUrl(this.properties.getPublicStorageUrl());
        return file;
    }
}
