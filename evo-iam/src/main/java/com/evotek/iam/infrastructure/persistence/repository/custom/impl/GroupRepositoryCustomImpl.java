package com.evotek.iam.infrastructure.persistence.repository.custom.impl;

import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.iam.domain.query.UserGroupSearchQuery;
import com.evotek.iam.infrastructure.persistence.entity.UserGroupEntity;
import com.evotek.iam.infrastructure.persistence.repository.custom.GroupRepositoryCustom;
import org.springframework.stereotype.Repository;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Repository
public class GroupRepositoryCustomImpl implements GroupRepositoryCustom {
    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public Long countGroup(UserGroupSearchQuery query) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        sql.append("select count(G) from UserGroupEntity G");
        sql.append(createWhereQuery(query, values));
        Query queryDB = entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(queryDB::setParameter);
        return (Long) queryDB.getSingleResult();
    }

    @Override
    public List<UserGroupEntity> search(UserGroupSearchQuery query) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        sql.append("select G from UserGroupEntity G");
        sql.append(createWhereQuery(query, values));
        sql.append(createOrderQuery(query.getSortBy()));
        Query queryDB = entityManager.createQuery(sql.toString(), UserGroupEntity.class);
        values.forEach(queryDB::setParameter);
        queryDB.setFirstResult((query.getPageIndex() - 1) * query.getPageSize());
        queryDB.setMaxResults(query.getPageSize());
        return queryDB.getResultList();
    }

    StringBuilder createWhereQuery(UserGroupSearchQuery query, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder();
        sql.append(" where G.deleted = false ");

        if (!StrUtils.isBlank(query.getKeyword())) {
            sql.append(" and ( G.name like :keyword" +
                    " or G.code like :keyword " +
                    " or G.description like :keyword) ");
            values.put("keyword", SqlUtils.encodeKeyword(query.getKeyword()));
        }
        return sql;
    }

    public StringBuilder createOrderQuery(String sortBy) {
        StringBuilder hql = new StringBuilder(" ");
        if (StringUtils.hasLength(sortBy)) {
            hql.append(" order by G.").append(sortBy.replace(".", " "));
        } else {
            hql.append(" order by G.name asc ");
        }
        return hql;
    }

}
