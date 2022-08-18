package com.evotek.notification.application.mapper;

import com.evotek.common.enums.EventType;
import com.evotek.common.mapper.EntityMapper;
import com.evotek.common.util.StrUtils;
import com.evotek.common.util.StringPool;
import com.evotek.notification.domain.Event;
import com.evotek.notification.infrastructure.persistence.entity.EventEntity;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import org.springframework.util.CollectionUtils;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring")
public interface EventEntityMapper extends EntityMapper<Event, EventEntity> {

    @Mapping(source = "eventEntity", target = "types", qualifiedByName = "typeToSet")
    Event toDomain(EventEntity eventEntity);


    @Mapping(source = "event", target = "types", qualifiedByName = "typeToString")
    EventEntity toEntity(Event event);

    @Named("typeToString")
    default String typeToString(Event event) {
        if (!CollectionUtils.isEmpty(event.getTypes())) {
            List<String> types = event.getTypes().stream().map(Enum::name).collect(Collectors.toList());
            return String.join(StringPool.COMMA, types);
        }
        return null;
    }

    @Named("typeToSet")
    default Set<EventType> typeToSet(EventEntity eventEntity) {
        if (!StrUtils.isBlank(eventEntity.getTypes())) {
            Set<EventType> types = new HashSet<>();
            List<String> typesStr = List.of(eventEntity.getTypes().split(StringPool.COMMA));
            typesStr.forEach(type -> types.add(EventType.valueOf(type)));
            return types;
        }
        return Collections.emptySet();
    }

}
