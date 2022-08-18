package com.evotek.iam.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.iam.domain.Client;
import com.evotek.iam.infrastructure.persistence.entity.ClientEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface ClientEntityMapper extends EntityMapper<Client, ClientEntity> {
}
