package com.evotek.notification.application.mapper;

import com.evotek.notification.application.dto.request.EventSearchRequest;
import com.evotek.notification.application.dto.request.NotificationSearchRequest;
import com.evotek.notification.domain.query.EventSearchQuery;
import com.evotek.notification.domain.query.NotificationSearchQuery;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface AutoMapperQuery {
    EventSearchQuery toQuery(EventSearchRequest eventSearchRequest);

    NotificationSearchQuery toQuery(NotificationSearchRequest notificationSearchRequest);
}
