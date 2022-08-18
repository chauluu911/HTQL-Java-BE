package com.evotek.iam.infrastructure.persistence.repository.custom.impl;

import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.DataUtil;
import com.evotek.common.util.StrUtils;
import com.evotek.iam.domain.query.ClientSearchQuery;
import com.evotek.iam.infrastructure.persistence.entity.ClientEntity;
import com.evotek.iam.infrastructure.persistence.repository.custom.ClientRepositoryCustom;
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

@Repository
public class ClientRepositoryCustomImpl implements ClientRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<ClientEntity> searchClient(ClientSearchQuery searchQuery) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder hql = new StringBuilder("SELECT c FROM ClientEntity c ");
        hql.append(this.createWhereQuery(searchQuery, values));
        hql.append(this.createOrderQuery(searchQuery));
        Query query = entityManager.createQuery(hql.toString(), ClientEntity.class);
        values.forEach((key, value) -> query.setParameter(key, value));
        query.setMaxResults(searchQuery.getPageSize());
        query.setFirstResult((searchQuery.getPageIndex() - 1) * searchQuery.getPageSize());
        return query.getResultList();
    }

    @Override
    public Long count(ClientSearchQuery searchQuery) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder hql = new StringBuilder("SELECT COUNT(c) FROM ClientEntity c ");
        Query query = entityManager.createQuery(hql.toString(), Long.class);
        values.forEach(query::setParameter);

        return DataUtil.getValueOrDefault((Long) query.getSingleResult(), 0L);
    }

    private StringBuilder createWhereQuery(ClientSearchQuery searchQuery, Map<String, Object> values) {
        StringBuilder hql = new StringBuilder(" WHERE c.deleted = false ");
        if (StringUtils.hasLength(searchQuery.getKeyword())) {
            hql.append(" and (c.name like :keyword)");
            values.put("keyword", SqlUtils.encodeKeyword(searchQuery.getKeyword()));
        }
        if (Objects.nonNull(searchQuery.getStatus())) {
            hql.append(" and c.status = :status");
            values.put("status", searchQuery.getStatus());
        }
        if (!CollectionUtils.isEmpty(searchQuery.getRoleIds())) {
            hql.append(" and c.roleId in (:roleIds)");
            values.put("roleIds", searchQuery.getRoleIds());
        }
        return hql;
    }

    private StringBuilder createOrderQuery(ClientSearchQuery searchQuery) {
        StringBuilder order = new StringBuilder();
        if (!StrUtils.isBlank(searchQuery.getSortBy())) {
            order.append(" order by c.").append(searchQuery.getSortBy().replace(".", " "));
        } else {
            order.append(" order by c.createdAt desc");
        }
        return order;
    }
}
