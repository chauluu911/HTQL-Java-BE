package com.evotek.order.infrastructure.repository.impl;

import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.order.infrastructure.entity.ProductEntity;
import com.evotek.order.infrastructure.query.ProductSearchQuery;
import com.evotek.order.infrastructure.repository.custom.ProductRepositoryCustom;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class ProductRepositoryImpl implements ProductRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<ProductEntity> search(ProductSearchQuery searchQuery) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder("SELECT P FROM ProductEntity P ");
        sql.append(createWhereQuery(searchQuery, values));
        sql.append(createOrderQuery(searchQuery.getSortBy()));
        Query query = entityManager.createQuery(sql.toString(), ProductEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((searchQuery.getPageIndex() - 1) * searchQuery.getPageSize());
        query.setMaxResults(searchQuery.getPageSize());
        return query.getResultList();
    }

    @Override
    public Long countProduct(ProductSearchQuery searchQuery) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder("SELECT COUNT(P) FROM ProductEntity P ");
        sql.append(createWhereQuery(searchQuery, values));
        Query query = entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);
        return (Long) query.getSingleResult();
    }

    public String createWhereQuery(ProductSearchQuery query, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder(" WHERE 1 = 1 ");
        if (!StrUtils.isBlank(query.getName())) {
            sql.append(" AND P.name like :name ");
            values.put("name", SqlUtils.encodeKeyword(query.getName()));
        }
        if (Objects.nonNull(query.getType())) {
            sql.append(" AND P.type = :type ");
            values.put("type", query.getType());
        }
        if (Objects.nonNull(query.getStartPrice())) {
            sql.append(" AND P.price >= :startPrice ");
            values.put("startPrice", query.getStartPrice());
        }
        if (Objects.nonNull(query.getEndPrice())) {
            sql.append(" AND P.price <= :endPrice ");
            values.put("endPrice", query.getEndPrice());
        }
        if (!StrUtils.isBlank(query.getKeyword())) {
            sql.append(" AND ( P.name like :nameKey ");
            values.put("nameKey", SqlUtils.encodeKeyword(query.getKeyword()));
            sql.append(" OR P.code like :codeKey ");
            values.put("codeKey", SqlUtils.encodeKeyword(query.getKeyword()));
            sql.append(" OR CAST(P.price AS text) like :priceKey )");
            values.put("priceKey", SqlUtils.encodeKeyword(query.getKeyword()));
        }
        sql.append(" AND P.deleted = :delete ");
        values.put("delete", Boolean.FALSE);
        return sql.toString();
    }

    private StringBuilder createOrderQuery(String sortBy) {
        StringBuilder sql = new StringBuilder(" ");
        if (StringUtils.hasLength(sortBy)) {
            sql.append(" order by P.").append(sortBy.replace(".", " "));
        } else {
            sql.append(" order by P.code desc ");
        }
        return sql;
    }
}
