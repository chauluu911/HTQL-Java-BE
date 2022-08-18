package com.evotek.iam.infrastructure.persistence.repository.custom.impl;

import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.iam.application.dto.request.RoleSearchRequest;
import com.evotek.iam.infrastructure.persistence.entity.RoleEntity;
import com.evotek.iam.infrastructure.persistence.repository.custom.RoleRepositoryCustom;
import com.evotek.iam.infrastructure.support.enums.RoleStatus;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Slf4j
@Repository
public class RoleRepositoryCustomImpl implements RoleRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<RoleEntity> search(RoleSearchRequest request) {
        StringBuilder sql = new StringBuilder("SELECT R FROM RoleEntity R ");
        Map<String, Object> values = new HashMap<>();
        sql.append(this.createWhereQuery(request, values));
        if (Objects.nonNull(request.getSortBy()) && !request.getSortBy().trim().equals("")) {
            sql.append(this.createOrderQuery(request));
        }
        Query query = this.entityManager.createQuery(sql.toString(), RoleEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((request.getPageIndex() - 1) * request.getPageSize());
        query.setMaxResults(request.getPageSize());
        return query.getResultList();
    }

    @Override
    public Long count(RoleSearchRequest request) {
        StringBuilder sql = new StringBuilder("SELECT COUNT(R) FROM RoleEntity R ");
        Map<String, Object> values = new HashMap<>();
        sql.append(this.createWhereQuery(request, values));
        Query query = this.entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);

        return (Long) query.getSingleResult();
    }

    public String createWhereQuery(RoleSearchRequest request, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder("WHERE 1 = 1 ");
        if (!StrUtils.isBlank(request.getKeyword())) {
            sql.append(" AND ( R.name LIKE :keyword OR R.code LIKE :keyword ) ");
            values.put("keyword", SqlUtils.encodeKeyword(request.getKeyword()));
        }

        if (!StrUtils.isBlank(request.getIsRoot())) {
            sql.append(" and R.isRoot = :isRoot ");
            values.put("isRoot", Boolean.valueOf(request.getIsRoot()));
        }

        if (!CollectionUtils.isEmpty(request.getCreatedBy())) {
            sql.append(" and R.createdBy in  (:createdBy) ");
            values.put("createdBy", request.getCreatedBy());
        }

        if (!StrUtils.isBlank(request.getStatus())) {
            sql.append(" and R.status = :status ");
            values.put("status", RoleStatus.valueOf(request.getStatus()));
        }

        if (Objects.nonNull(request.getStartAt()) && Objects.nonNull(request.getEndAt())) {
            sql.append(" AND ( ( R.createdAt <= :startAt ");
            sql.append(" AND R.lastModifiedAt >= :startAt) ");
            sql.append(" OR ( R.createdAt <= :endAt ");
            sql.append(" AND R.lastModifiedAt >= :endAt )");
            sql.append(" OR ( R.createdAt >= :startAt ");
            sql.append(" AND R.lastModifiedAt <= :endAt )");
            sql.append(" ) ");
            values.put("startAt", request.getStartAt());
            values.put("endAt", request.getEndAt());
        } else if (Objects.nonNull(request.getStartAt()) && Objects.isNull(request.getEndAt())) {
            sql.append(" AND R.createdAt >= :startAt ");
            values.put("startAt", request.getStartAt());
        } else if (Objects.isNull(request.getStartAt()) && Objects.nonNull(request.getEndAt())) {
            sql.append(" AND R.lastModifiedAt >= :endAt ");
            values.put("endAt", request.getEndAt());
        }

        if (Objects.nonNull(request.getStartCreatedAt())) {
            sql.append(" AND R.createdAt >= :startCreatedAt");
            values.put("startCreatedAt", request.getStartCreatedAt());
        }
        if (Objects.nonNull(request.getEndCreatedAt())) {
            sql.append(" AND R.createdAt <= :endCreatedAt");
            values.put("endCreatedAt", request.getEndCreatedAt());
        }

        if (Objects.nonNull(request.getStartLastModifiedAt())) {
            sql.append(" AND R.lastModifiedAt >= :startLastModifiedAt");
            values.put("startLastModifiedAt", request.getStartLastModifiedAt());
        }
        if (Objects.nonNull(request.getEndLastModifiedAt())) {
            sql.append(" AND R.lastModifiedAt <= :endLastModifiedAt");
            values.put("endLastModifiedAt", request.getEndLastModifiedAt());
        }

        sql.append(" AND R.deleted = FALSE ");
        return sql.toString();
    }

    public StringBuilder createOrderQuery(RoleSearchRequest searchRequest) {
        StringBuilder sql = new StringBuilder(" ");
        if (StringUtils.hasLength(searchRequest.getSortBy())) {
            sql.append(" order by R.").append(searchRequest.getSortBy().replace(".", " "));
        }
        return sql;
    }
}
