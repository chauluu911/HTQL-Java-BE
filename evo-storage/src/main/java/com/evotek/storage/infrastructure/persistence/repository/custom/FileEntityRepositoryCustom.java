package com.evotek.storage.infrastructure.persistence.repository.custom;

import com.evotek.storage.infrastructure.persistence.entity.FileEntity;
import com.evotek.storage.domain.query.FileSearchQuery;

import java.util.List;

public interface FileEntityRepositoryCustom {

    List<FileEntity> search(FileSearchQuery query);

    Long count(FileSearchQuery query);
}
