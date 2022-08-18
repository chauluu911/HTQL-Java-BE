package com.evotek.iam.infrastructure.persistence.repository.custom.impl;

import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.iam.domain.query.DepartmentSearchQuery;
import com.evotek.iam.infrastructure.persistence.entity.DepartmentEntity;
import com.evotek.iam.infrastructure.persistence.repository.custom.DepartmentRepositoryCustom;
import com.evotek.iam.infrastructure.support.enums.DepartmentStatus;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

@Slf4j
@Repository
public class DepartmentRepositoryCustomImpl implements DepartmentRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<DepartmentEntity> search(DepartmentSearchQuery request) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder("SELECT D FROM DepartmentEntity D ");
        sql.append(this.createWhereQuery(request, values));
        if (Objects.nonNull(request.getSortBy()) && !request.getSortBy().trim().equals("")) {
            sql.append(this.createOrderQuery(request));
        }
        Query query = this.entityManager.createQuery(sql.toString(), DepartmentEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((request.getPageIndex() - 1) * request.getPageSize());
        query.setMaxResults(request.getPageSize());
        return query.getResultList();
    }

    @Override
    public Long count(DepartmentSearchQuery request) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder("SELECT COUNT(D) FROM DepartmentEntity D ");
        sql.append(this.createWhereQuery(request, values));
        Query query = this.entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);
        return (Long) query.getSingleResult();
    }

    @Override
    public List<DepartmentEntity> getChildDepartmentsByParentIds(List<String> parentIds) {
        StringBuilder sql = new StringBuilder();
        Map<String, Object> values = new HashMap<>();

        sql.append("SELECT d FROM DepartmentEntity d WHERE d.deleted = false ");

        if (!CollectionUtils.isEmpty(parentIds)) {
            sql.append(" and ( ");
            AtomicInteger atomicInteger = new AtomicInteger();
            parentIds.forEach(id -> {
                int index = atomicInteger.addAndGet(1);

                sql.append(" (d.parentPath like :departmentParentId").append(index).append(" OR d.id = :departmentId").append(index).append(" ) ");
                if (index < parentIds.size()) {
                    sql.append(" or ");
                }
                values.put("departmentParentId" + index, SqlUtils.encodeKeyword(id));
                values.put("departmentId" + index, id);
            });
            sql.append(") ");

        } else {
            return new ArrayList<>();
        }
        log.info("sql => {}", sql);

        Query query = entityManager.createQuery(sql.toString(), DepartmentEntity.class);
        values.forEach(query::setParameter);
        return query.getResultList();
    }

    public String createWhereQuery(DepartmentSearchQuery request, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder("WHERE 1 = 1 ");
        if (!StrUtils.isBlank(request.getKeyword())) {
            sql.append(" AND ( LOWER(D.name) LIKE :keyword OR LOWER(D.code) LIKE :keyword   ) ");
            values.put("keyword", SqlUtils.encodeKeyword(request.getKeyword()));
        }
        if (Objects.nonNull(request.getStartAt()) && Objects.nonNull(request.getEndAt())) {
            sql.append(" AND ( ( D.createdAt <= :startAt ");
            sql.append(" AND D.lastModifiedAt >= :startAt) ");
            sql.append(" OR ( D.createdAt <= :endAt ");
            sql.append(" AND D.lastModifiedAt >= :endAt )");
            sql.append(" OR ( D.createdAt >= :startAt ");
            sql.append(" AND D.lastModifiedAt <= :endAt )");
            sql.append(" ) ");
            values.put("startAt", request.getStartAt());
            values.put("endAt", request.getEndAt());
        } else if (Objects.nonNull(request.getStartAt()) && Objects.isNull(request.getEndAt())) {
            sql.append(" AND D.createdAt >= :startAt ");
            values.put("startAt", request.getStartAt());
        } else if (Objects.isNull(request.getStartAt()) && Objects.nonNull(request.getEndAt())) {
            sql.append(" AND D.lastModifiedAt >= :endAt ");
            values.put("endAt", request.getEndAt());
        }

        if (Objects.nonNull(request.getStartCreatedAt())) {
            sql.append(" AND D.createdAt >= :startCreatedAt");
            values.put("startCreatedAt", request.getStartCreatedAt());
        }
        if (Objects.nonNull(request.getEndCreatedAt())) {
            sql.append(" AND D.createdAt <= :endCreatedAt");
            values.put("endCreatedAt", request.getEndCreatedAt());
        }

        if (Objects.nonNull(request.getStartLastModifiedAt())) {
            sql.append(" AND D.lastModifiedAt >= :startLastModifiedAt");
            values.put("startLastModifiedAt", request.getStartLastModifiedAt());
        }
        if (Objects.nonNull(request.getEndLastModifiedAt())) {
            sql.append(" AND D.lastModifiedAt <= :endLastModifiedAt");
            values.put("endLastModifiedAt", request.getEndLastModifiedAt());
        }

        if (!StrUtils.isBlank(request.getStatus())) {
            sql.append(" and D.status = :status ");
            values.put("status", DepartmentStatus.valueOf(request.getStatus()));
        }

        sql.append(" AND D.deleted = FALSE ");
        return sql.toString();
    }

    public StringBuilder createOrderQuery(DepartmentSearchQuery searchRequest) {
        StringBuilder sql = new StringBuilder(" ");
        if (StringUtils.hasLength(searchRequest.getSortBy())) {
            sql.append(" order by D.").append(searchRequest.getSortBy().replace(".", " "));
        }
        return sql;
    }
}
