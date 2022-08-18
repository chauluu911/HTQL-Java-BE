package com.evotek.meet.infrastructure.persistence.repository.impl;

import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.meet.application.dto.request.RoomSearchRequest;
import com.evotek.meet.infrastructure.persistence.entity.RoomEntity;
import com.evotek.meet.infrastructure.persistence.repository.custom.RoomEntityRepositoryCustom;
import lombok.extern.slf4j.Slf4j;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Slf4j
public class RoomEntityRepositoryImpl implements RoomEntityRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<RoomEntity> search(RoomSearchRequest searchRequest) {
        StringBuilder sql = new StringBuilder("SELECT r FROM RoomEntity r ");
        Map<String, Object> values = new HashMap<>();
        sql.append(this.createWhereQuery(searchRequest, values));
        sql.append(this.createOrderQuery(searchRequest));
        Query query = this.entityManager.createQuery(sql.toString(), RoomEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((searchRequest.getPageIndex() - 1) * searchRequest.getPageSize());
        query.setMaxResults(searchRequest.getPageSize());
        return query.getResultList();
    }

    @Override
    public Long count(RoomSearchRequest searchRequest) {
        StringBuilder sql = new StringBuilder("SELECT COUNT(r) FROM RoomEntity r ");
        Map<String, Object> values = new HashMap<>();
        sql.append(this.createWhereQuery(searchRequest, values));
        Query query = this.entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);

        return (Long) query.getSingleResult();
    }

    public String createWhereQuery(RoomSearchRequest searchRequest, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder(" WHERE r.deleted = false ");
        if (!StrUtils.isBlank(searchRequest.getKeyword())) {
            sql.append("AND ( r.name LIKE :keyword OR r.code LIKE :keyword ) ");
            values.put("keyword", SqlUtils.encodeKeyword(searchRequest.getKeyword()));
        }
        if (!StrUtils.isBlank(searchRequest.getName())) {
            sql.append(" AND r.name LIKE :name ");
            values.put("name", SqlUtils.encodeKeyword(searchRequest.getName()));
        }
        if (!StrUtils.isBlank(searchRequest.getLocation())) {
            sql.append(" AND r.location LIKE :location ");
            values.put("location", SqlUtils.encodeKeyword(searchRequest.getLocation()));
        }
        if (Objects.nonNull(searchRequest.getStatus())) {
            sql.append(" AND r.status = :status ");
            values.put("status", searchRequest.getStatus());
        }
        return sql.toString();
    }

    public StringBuilder createOrderQuery(RoomSearchRequest searchRequest) {
        StringBuilder sql = new StringBuilder(" ");
        if (!StrUtils.isBlank(searchRequest.getSortBy())) {
            sql.append(" order by r. ").append(searchRequest.getSortBy().replace(".", " "));
        } else {
            sql.append(" order by r.createdAt ");
        }
        return sql;
    }
}
