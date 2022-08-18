package com.evotek.order.infrastructure.repository.impl;

import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.order.infrastructure.entity.MenuEntity;
import com.evotek.order.infrastructure.query.MenuSearchQuery;
import com.evotek.order.infrastructure.repository.custom.MenuRepositoryCustom;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class MenuRepositoryImpl implements MenuRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<MenuEntity> search(MenuSearchQuery searchQuery) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder("SELECT M FROM MenuEntity M ");
        sql.append(createWhereQuery(searchQuery, values));
        sql.append(createOrderQuery(searchQuery.getSortBy()));
        Query query = entityManager.createQuery(sql.toString(), MenuEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((searchQuery.getPageIndex() - 1) * searchQuery.getPageSize());
        query.setMaxResults(searchQuery.getPageSize());
        return query.getResultList();
    }

    @Override
    public Long countMenu(MenuSearchQuery searchQuery) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder("SELECT COUNT(M) FROM MenuEntity M ");
        sql.append(createWhereQuery(searchQuery, values));
        Query query = entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);
        return (Long) query.getSingleResult();
    }

    private String createWhereQuery(MenuSearchQuery searchQuery, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder(" WHERE 1 = 1 ");
        if (!StrUtils.isBlank(searchQuery.getTitle())) {
            sql.append(" AND M.title like :title ");
            values.put("title", SqlUtils.encodeKeyword(searchQuery.getTitle()));
        }
        if (Objects.nonNull(searchQuery.getType())) {
            sql.append(" AND M.type = :type ");
            values.put("type", searchQuery.getType());
        }
        if (Objects.nonNull(searchQuery.getPublished())) {
            sql.append(" AND M.published = :published ");
            values.put("published", searchQuery.getPublished());
        }
        if (Objects.nonNull(searchQuery.getStartClosedAt())) {
            sql.append(" AND M.closedAt >= :startClosedAt ");
            values.put("startClosedAt", searchQuery.getStartClosedAt());
        }
        if (Objects.nonNull(searchQuery.getEndClosedAt())) {
            sql.append(" AND M.closedAt <= :endClosedAt ");
            values.put("endClosedAt", searchQuery.getEndClosedAt());
        }
        if (!StrUtils.isBlank(searchQuery.getKeyword())) {
            sql.append(" AND ( M.code like :codeKey ");
            values.put("codeKey", SqlUtils.encodeKeyword(searchQuery.getKeyword()));
            sql.append(" OR M.title like :titleKey ");
            values.put("titleKey", SqlUtils.encodeKeyword(searchQuery.getKeyword()));
            sql.append(" OR M.note like :noteKey ");
            values.put("noteKey", SqlUtils.encodeKeyword(searchQuery.getKeyword()));
            sql.append(" OR CAST(M.price AS text) like :priceKey )");
            values.put("priceKey", SqlUtils.encodeKeyword(searchQuery.getKeyword()));
        }
        sql.append(" AND M.deleted = :delete ");
        values.put("delete", Boolean.FALSE);
        return sql.toString();
    }

    private StringBuilder createOrderQuery(String sortBy) {
        StringBuilder sql = new StringBuilder(" ");
        if (StringUtils.hasLength(sortBy)) {
            sql.append(" order by M.").append(sortBy.replace(".", " "));
        } else {
            sql.append(" order by M.code desc ");
        }
        return sql;
    }
}
