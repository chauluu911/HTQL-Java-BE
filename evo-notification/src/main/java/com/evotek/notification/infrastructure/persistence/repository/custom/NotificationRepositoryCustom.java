package com.evotek.notification.infrastructure.persistence.repository.custom;

import com.evotek.notification.domain.query.NotificationSearchQuery;
import com.evotek.notification.infrastructure.persistence.entity.NotificationEntity;

import java.util.List;

public interface NotificationRepositoryCustom {

    List<NotificationEntity> search(NotificationSearchQuery params);

    Long count(NotificationSearchQuery params);

}
