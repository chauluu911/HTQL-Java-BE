package com.evotek.order.infrastructure.repository.impl;

import com.evotek.common.util.StrUtils;
import com.evotek.order.infrastructure.entity.PurchaseOrderEntity;
import com.evotek.order.infrastructure.query.PurchaseOrderSearchQuery;
import com.evotek.order.infrastructure.repository.custom.PurchaseOrderRepositoryCustom;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class PurchaseOrderRepositoryImpl implements PurchaseOrderRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<PurchaseOrderEntity> search(PurchaseOrderSearchQuery searchQuery) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder("SELECT O FROM PurchaseOrderEntity O ");
        sql.append(createWhereQuery(searchQuery, values));
        sql.append(createOrderQuery(searchQuery.getSortBy()));
        Query query = entityManager.createQuery(sql.toString(), PurchaseOrderEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((searchQuery.getPageIndex() - 1) * searchQuery.getPageSize());
        query.setMaxResults(searchQuery.getPageSize());
        return query.getResultList();
    }

    @Override
    public Long countOrder(PurchaseOrderSearchQuery searchQuery) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder("SELECT COUNT(O) FROM PurchaseOrderEntity O ");
        sql.append(createWhereQuery(searchQuery, values));
        Query query = entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);
        return (Long) query.getSingleResult();
    }

    private String createWhereQuery(PurchaseOrderSearchQuery searchQuery, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder(" WHERE 1 = 1");
        if (!StrUtils.isBlank(searchQuery.getCreatedUserId())) {
            sql.append(" AND ( O.createdUserId = :createdUserId OR O.ownerId = :createdUserId ) ");
            values.put("createdUserId", searchQuery.getCreatedUserId());
        } else if (!StrUtils.isBlank(searchQuery.getUserId())) {
            sql.append(" AND ( O.createdUserId = :userId OR O.ownerId = :userId ) ");
            values.put("userId", searchQuery.getUserId());
        }
        if (Objects.nonNull(searchQuery.getType())) {
            sql.append(" AND O.type = :type");
            values.put("type", searchQuery.getType());
        }
        if (Objects.nonNull(searchQuery.getStatus())) {
            sql.append(" AND O.status = :status");
            values.put("status", searchQuery.getStatus());
        }
        if (Objects.nonNull(searchQuery.getStartCreatedAt())) {
            sql.append(" AND O.createdAt >= :startCreatedAt");
            values.put("startCreatedAt", searchQuery.getStartCreatedAt());
        }
        if (Objects.nonNull(searchQuery.getEndCreatedAt())) {
            sql.append(" AND O.createdAt <= :endCreatedAt");
            values.put("endCreatedAt", searchQuery.getEndCreatedAt());
        }
        if (!StrUtils.isBlank(searchQuery.getMenuId())) {
            sql.append(" AND O.menuId = :menuId");
            values.put("menuId", searchQuery.getMenuId());
        }
        sql.append(" AND O.deleted = :delete ");
        values.put("delete", Boolean.FALSE);
        return sql.toString();
    }

    private StringBuilder createOrderQuery(String sortBy) {
        StringBuilder sql = new StringBuilder(" ");
        if (StringUtils.hasLength(sortBy)) {
            sql.append(" order by O.").append(sortBy.replace(".", " "));
        } else {
            sql.append(" order by O.code desc ");
        }
        return sql;
    }
}
