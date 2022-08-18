package com.evotek.asset.infrastructure.persistence.repository.custom.impl;

import com.evotek.asset.domain.query.AssetSearchQuery;
import com.evotek.asset.domain.query.AssetTypeSearchQuery;
import com.evotek.asset.infrastructure.persistence.entity.AssetEntity;
import com.evotek.asset.infrastructure.persistence.repository.custom.AssetRepositoryCustom;
import com.evotek.common.persistence.support.SqlUtils;
import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Repository
public class AssetRepositoryCustomImpl implements AssetRepositoryCustom {
    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<AssetEntity> search(AssetSearchQuery querySearch) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        sql.append("SELECT ae FROM AssetEntity ae ");
        sql.append(createWhereQuery(querySearch, values));
        sql.append(createOrderQuery(querySearch));
        Query query = entityManager.createQuery(sql.toString(), AssetEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((querySearch.getPageIndex() - 1) * querySearch.getPageSize());
        query.setMaxResults(querySearch.getPageSize());
        return query.getResultList();
    }

    @Override
    public Long count(AssetSearchQuery querySearch) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        sql.append("SELECT count(ae) FROM AssetEntity ae ");
        sql.append(createWhereQuery(querySearch, values));
        Query query = entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);
        return (Long) query.getSingleResult();
    }

    @Override
    public List<AssetEntity> searchAutoComplete(AssetSearchQuery querySearch) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        sql.append("SELECT ae FROM AssetEntity ae ");
        sql.append(createWhereQuery(querySearch, values));
        sql.append(createOrderQuery(querySearch));
        Query query = entityManager.createQuery(sql.toString(), AssetEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((querySearch.getPageIndex() - 1) * querySearch.getPageSize());
        query.setMaxResults(querySearch.getPageSize());
        return query.getResultList();
    }

    StringBuilder createWhereQuery(AssetSearchQuery query, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder();
        sql.append(" WHERE ae.deleted = false ");
        if (Objects.nonNull(query.getKeyword())) {
            sql.append("and ( ae.code like :keyword " +
                    " or ae.name like :keyword )");
            values.put("keyword", SqlUtils.encodeKeyword(query.getKeyword()));
        }
        if (Objects.nonNull(query.getStatus())) {
            sql.append(" and ae.status = :status");
            values.put("status", query.getStatus());
        }
        return sql;
    }

    StringBuilder createOrderQuery(AssetSearchQuery query) {
        StringBuilder sql = new StringBuilder(" ");
        if (StringUtils.hasLength(query.getSortBy())) {
            sql.append(" order by ae.").append(query.getSortBy().replace(".", " "));
        }
        return sql;
    }
}
