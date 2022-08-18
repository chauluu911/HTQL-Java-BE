package com.evotek.storage.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.storage.domain.File;
import com.evotek.storage.infrastructure.persistence.entity.FileEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface FileEntityMapper extends EntityMapper<File, FileEntity> {
}
