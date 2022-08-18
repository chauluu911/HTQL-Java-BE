package com.evotek.notification.infrastructure.persistence.repository.impl;

import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.notification.domain.query.EventSearchQuery;
import com.evotek.notification.infrastructure.persistence.entity.EventEntity;
import com.evotek.notification.infrastructure.persistence.repository.custom.EventRepositoryCustom;
import com.evotek.notification.infrastructure.support.enums.EventSource;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class EventRepositoryImpl implements EventRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<EventEntity> search(EventSearchQuery request) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder("FROM EventEntity E ");
        sql.append(createWhereQuery(request, values));
        sql.append(createOrderQuery(request));
        Query query = entityManager.createQuery(sql.toString(), EventEntity.class);
        values.forEach(query::setParameter);

        query.setFirstResult((request.getPageIndex() - 1) * request.getPageSize());
        query.setMaxResults(request.getPageSize());
        return query.getResultList();
    }

    @Override
    public Long countEvent(EventSearchQuery request) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder("SELECT count(E.id) FROM EventEntity E ");
        sql.append(createWhereQuery(request, values));
        Query query = entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);
        return (Long) query.getSingleResult();
    }

    String createWhereQuery(EventSearchQuery request, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder(" where 1 = 1 ");
        if (Objects.nonNull(request.getStatus())) {
            sql.append(" and E.status = :status ");
            values.put("status", request.getStatus());
        }
        if (!StrUtils.isBlank(request.getKeyword())) {
            sql.append(" and ( (E.title like :keyword)" +
                    " or (E.note like :keyword)" +
                    " or (E.description like :keyword) ) ");
            values.put("keyword", SqlUtils.encodeKeyword(request.getKeyword()));
        }
        if (Objects.nonNull(request.getExpectedNotificationStartAt())) {
            sql.append(" and E.expectedNotificationAt >= :expectedStartDate ");
            values.put("expectedStartDate", request.getExpectedNotificationStartAt());
        }

        if (Objects.nonNull(request.getExpectedNotificationEndAt())) {
            sql.append(" and E.expectedNotificationAt <= :expectedEndDate ");
            values.put("expectedEndDate", request.getExpectedNotificationEndAt());
        }

        if (Objects.nonNull(request.getNotificationStartAt())) {
            sql.append(" and E.notificationAt >= :notificationStartAt ");
            values.put("notificationStartAt", request.getNotificationStartAt());
        }

        if (Objects.nonNull(request.getNotificationEndAt())) {
            sql.append(" and E.notificationAt <= :notificationEndAt ");
            values.put("notificationEndAt", request.getNotificationEndAt());
        }

        if (Objects.nonNull(request.getSendTo())) {
            sql.append(" and E.sendTo = :sendTo ");
            values.put("sendTo", request.getSendTo());
        }

        if (!StrUtils.isBlank(request.getIssuedUserId())) {
            sql.append(" and E.issuedUserId = :issuedUserId ");
            values.put("issuedUserId", request.getIssuedUserId());
        }

        sql.append(" and E.deleted = false ");
        sql.append(" and E.eventSource = :eventSource");
        values.put("eventSource", EventSource.USER);

        return sql.toString();
    }

    public StringBuilder createOrderQuery(EventSearchQuery params) {
        StringBuilder sql = new StringBuilder(" ");
        if (StringUtils.hasLength(params.getSortBy())) {
            sql.append(" order by E.").append(params.getSortBy().replace(".", " "));
        } else {
            sql.append(" order by E.createdAt desc ");
        }
        return sql;
    }
}
