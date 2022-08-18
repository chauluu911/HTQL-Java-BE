package com.evotek.asset.infrastructure.persistence.repository.custom.impl;

import com.evotek.asset.domain.query.AssetSearchQuery;
import com.evotek.asset.domain.query.AssetTypeSearchQuery;
import com.evotek.asset.infrastructure.persistence.entity.AssetTypeEntity;
import com.evotek.asset.infrastructure.persistence.repository.custom.AssetTypeRepositoryCustom;
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
public class AssetTypeRepositoryCustomImpl implements AssetTypeRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<AssetTypeEntity> search(AssetTypeSearchQuery querySearch) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        sql.append("select ate from AssetTypeEntity ate ");
        sql.append(createWhereQuery(querySearch, values));
        sql.append(createOrderQuery(querySearch));
        Query query = entityManager.createQuery(sql.toString(), AssetTypeEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((querySearch.getPageIndex() - 1) * querySearch.getPageSize());
        query.setMaxResults(querySearch.getPageSize());
        return query.getResultList();

    }

    @Override
    public Long count(AssetTypeSearchQuery querySearch) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        sql.append("SELECT count(ate) FROM AssetTypeEntity ate ");
        sql.append(createWhereQuery(querySearch, values));
        Query query = entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);
        return (Long) query.getSingleResult();
    }

    @Override
    public List<AssetTypeEntity> searchAutoComplete(AssetTypeSearchQuery querySearch) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        sql.append("select ate from AssetTypeEntity ate ");
        sql.append(createWhereQuery(querySearch, values));
        sql.append(createOrderQuery(querySearch));
        Query query = entityManager.createQuery(sql.toString(), AssetTypeEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((querySearch.getPageIndex() - 1) * querySearch.getPageSize());
        query.setMaxResults(querySearch.getPageSize());
        return query.getResultList();
    }


    StringBuilder createWhereQuery(AssetTypeSearchQuery query, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder();
        sql.append(" WHERE ate.deleted = false ");
        if (Objects.nonNull(query.getKeyword())) {
            sql.append("and ( ate.code like :keyword " +
                    " or ate.name like :keyword )");
            values.put("keyword", SqlUtils.encodeKeyword(query.getKeyword()));
        }
        if (Objects.nonNull(query.getStatus())) {
            sql.append(" and ate.status = :status");
            values.put("status", query.getStatus());
        }
        return sql;
    }

    StringBuilder createOrderQuery(AssetTypeSearchQuery query) {
        StringBuilder sql = new StringBuilder(" ");
        if (StringUtils.hasLength(query.getSortBy())) {
            sql.append(" order by ate.").append(query.getSortBy().replace(".", " "));
        }
        return sql;
    }
}
