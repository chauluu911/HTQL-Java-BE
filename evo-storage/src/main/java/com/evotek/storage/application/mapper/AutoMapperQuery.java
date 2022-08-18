package com.evotek.storage.application.mapper;

import com.evotek.storage.application.dto.request.FileSearchRequest;
import com.evotek.storage.domain.query.FileSearchQuery;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface AutoMapperQuery {

    FileSearchQuery from(FileSearchRequest request);
}
