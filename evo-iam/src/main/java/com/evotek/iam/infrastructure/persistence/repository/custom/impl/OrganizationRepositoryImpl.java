package com.evotek.iam.infrastructure.persistence.repository.custom.impl;

import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.iam.infrastructure.persistence.entity.OrganizationEntity;
import com.evotek.iam.domain.query.OrganizationSearchQuery;
import com.evotek.iam.infrastructure.persistence.repository.custom.OrganizationRepositoryCustom;
import com.evotek.iam.infrastructure.support.enums.OrganizationStatus;
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
public class OrganizationRepositoryImpl implements OrganizationRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<OrganizationEntity> search(OrganizationSearchQuery querySearch) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder("SELECT O FROM OrganizationEntity O ");
        sql.append(createWhereQuery(querySearch, values));
        sql.append(createOrderQuery(querySearch.getSortBy()));
        Query query = this.entityManager.createQuery(sql.toString(), OrganizationEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((querySearch.getPageIndex() - 1) * querySearch.getPageSize());
        query.setMaxResults(querySearch.getPageSize());
        return query.getResultList();
    }

    StringBuilder createWhereQuery(OrganizationSearchQuery querySearch, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder(" where 1 = 1 ");
        if (!StrUtils.isBlank(querySearch.getKeyword())) {
            sql.append(" and ( O.name like :keyword or O.code like :keyword" +
                    " or O.email like :keyword or O.phoneNumber like :keyword " +
                    " or O.businessCode like :keyword" +
                    " or O.legalRepresentative like :keyword )");
            values.put("keyword", SqlUtils.encodeKeyword(querySearch.getKeyword()));
        }

        if (!StrUtils.isBlank(querySearch.getStatus())) {
            sql.append(" and O.status = :status ");
            values.put("status", OrganizationStatus.valueOf(querySearch.getStatus()));
        }

        if (Objects.nonNull(querySearch.getIncorporationDate())) {
            sql.append(" and O.incorporationDate = :incorporationDate ");
            values.put("incorporationDate", querySearch.getIncorporationDate());
        }
        sql.append("and O.deleted = false");
        return new StringBuilder(sql.toString());
    }

    @Override
    public Long countOrganization(OrganizationSearchQuery querySearch) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder("SELECT count(O) FROM OrganizationEntity O ");
        sql.append(createWhereQuery(querySearch, values));
        Query query = this.entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);
        return (Long) query.getSingleResult();
    }

    public StringBuilder createOrderQuery(String sortBy) {
        StringBuilder sql = new StringBuilder(" ");
        if (StringUtils.hasLength(sortBy)) {
            sql.append(" order by O.").append(sortBy.replace(".", " "));
        }
        return sql;
    }
}

