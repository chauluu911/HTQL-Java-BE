package com.evotek.iam.infrastructure.persistence.repository.custom.impl;

import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.iam.domain.query.JobTitleSearchQuery;
import com.evotek.iam.infrastructure.persistence.entity.JobTitleEntity;
import com.evotek.iam.infrastructure.persistence.repository.custom.JobTitleRepositoryCustom;
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
public class JobTitleRepositoryCustomImpl implements JobTitleRepositoryCustom {
    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<JobTitleEntity> search(JobTitleSearchQuery querySearch) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        sql.append("select jte from JobTitleEntity jte ");
        sql.append(createWhereQuery(querySearch, values));
        sql.append(createOrderQuery(querySearch));
        Query query = entityManager.createQuery(sql.toString(), JobTitleEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((querySearch.getPageIndex() - 1) * querySearch.getPageSize());
        query.setMaxResults(querySearch.getPageSize());
        return query.getResultList();

    }

    @Override
    public Long count(JobTitleSearchQuery querySearch) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        sql.append("select count(jte) from JobTitleEntity jte ");
        sql.append(createWhereQuery(querySearch, values));
        Query query = entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);
        return  (Long) query.getSingleResult();


    }

    StringBuilder createWhereQuery(JobTitleSearchQuery query, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder();
        sql.append(" WHERE jte.deleted = false ");
        if (!StrUtils.isBlank(query.getKeyword())) {
            sql.append(" and ( jte.name like :keyword " +
                    " or jte.code like :keyword " +
                    " or jte.id like :keyword)");
            values.put("keyword", SqlUtils.encodeKeyword(query.getKeyword()));
        }
        if(Objects.nonNull(query.getStatus())) {
            sql.append(" and jte.status = :status");
            values.put("status", query.getStatus());
        }
            return sql;
    }
    public StringBuilder createOrderQuery(JobTitleSearchQuery query) {
        StringBuilder sql = new StringBuilder(" ");
        if (StringUtils.hasLength(query.getSortBy())) {
            sql.append(" order by jte.").append(query.getSortBy().replace(".", " "));
        }
        return sql;
    }


}
