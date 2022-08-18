package com.evotek.notification.infrastructure.persistence.repository.impl;

import com.evotek.common.util.StrUtils;
import com.evotek.notification.domain.query.NotificationSearchQuery;
import com.evotek.notification.infrastructure.persistence.entity.NotificationEntity;
import com.evotek.notification.infrastructure.persistence.repository.custom.NotificationRepositoryCustom;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class NotificationRepositoryImpl implements NotificationRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;


    @Override
    public List<NotificationEntity> search(NotificationSearchQuery params) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        sql.append(" SELECT e FROM NotificationEntity e ");
        sql.append(this.createWhereQuery(params, values));
        sql.append(this.createOrderQuery(params));
        Query query = entityManager.createQuery(sql.toString());
        values.forEach(query::setParameter);

        query.setFirstResult((params.getPageIndex() - 1) * params.getPageSize());
        query.setMaxResults(params.getPageSize());
        return query.getResultList();
    }

    @Override
    public Long count(NotificationSearchQuery params) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder("SELECT count(e) FROM NotificationEntity e ");
        sql.append(this.createWhereQuery(params, values));
        Query query = entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);
        return (Long) query.getSingleResult();
    }

    private String createWhereQuery(NotificationSearchQuery params, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder();
        sql.append(" WHERE 1 = 1 ");
        if (Objects.nonNull(params.getUserId()) && !StrUtils.isBlank(params.getUserId())) {
            sql.append(" and e.userId = :userId ");
            values.put("userId", params.getUserId());
        }
        if(Objects.nonNull(params.getIsRead())){
            sql.append("and e.isRead = :isRead");
            values.put("isRead", params.getIsRead());
        }
        if (Objects.nonNull(params.getSendStartAt())) {
            sql.append(" and e.sendAt >= :sendStartAt ");
            values.put("sendStartAt", params.getSendStartAt());
        }
        if (Objects.nonNull(params.getSendEndAt())) {
            sql.append(" and e.sendAt <= :sendEndAt ");
            values.put("sendEndAt", params.getSendEndAt());
        }
        sql.append(" and e.isSend = true ");
        sql.append(" and e.deleted = false ");
        return sql.toString();
    }

    public StringBuilder createOrderQuery(NotificationSearchQuery params) {
        StringBuilder sql = new StringBuilder(" ");
        if (StringUtils.hasLength(params.getSortBy())) {
            sql.append(" order by e.").append(params.getSortBy().replace(".", " "));
        } else {
            sql.append(" order by e.createdAt desc ");
        }
        return sql;
    }
}
