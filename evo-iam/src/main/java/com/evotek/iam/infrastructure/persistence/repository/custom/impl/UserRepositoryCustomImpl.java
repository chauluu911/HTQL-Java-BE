package com.evotek.iam.infrastructure.persistence.repository.custom.impl;

import com.evotek.common.enums.AccountType;
import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.iam.domain.query.EmployeeSearchQuery;
import com.evotek.iam.domain.query.UserSearchQuery;
import com.evotek.iam.infrastructure.persistence.entity.UserEntity;
import com.evotek.iam.infrastructure.persistence.repository.custom.UserRepositoryCustom;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
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
public class UserRepositoryCustomImpl implements UserRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<UserEntity> search(UserSearchQuery querySearch) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        if (!CollectionUtils.isEmpty(querySearch.getRoleIds())) {
            sql.append("select distinct U from UserEntity U");
            sql.append(" inner join UserRoleEntity UR on U.id = UR.userId");
        } else {
            sql.append("select U from UserEntity U");
        }
        sql.append(createWhereQuery(querySearch, values));
        sql.append(createOrderQuery(querySearch.getSortBy()));

        Query query = entityManager.createQuery(sql.toString(), UserEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((querySearch.getPageIndex() - 1) * querySearch.getPageSize());
        query.setMaxResults(querySearch.getPageSize());
        return query.getResultList();
    }

    @Override
    public Long countUser(UserSearchQuery querySearch) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        if (!CollectionUtils.isEmpty(querySearch.getRoleIds())) {
            sql.append("select count(distinct U) from UserEntity U");
            sql.append(" inner join UserRoleEntity UR on U.id = UR.userId");
        } else {
            sql.append("select count(U) from UserEntity U");
        }
        sql.append(createWhereQuery(querySearch, values));
        Query query = entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);
        return (Long) query.getSingleResult();
    }

    StringBuilder createWhereQuery(UserSearchQuery querySearch, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder();
        sql.append(" where U.deleted = false ");

        if (!StrUtils.isBlank(querySearch.getKeyword())) {
            sql.append(" and ( U.username like :keyword" +
                    " or U.email like :keyword" +
                    " or U.employeeCode like :keyword" +
                    " or U.fullName like :keyword" +
                    " or U.phoneNumber like :keyword  ) ");
            values.put("keyword", SqlUtils.encodeKeyword(querySearch.getKeyword()));
        }

        if (!CollectionUtils.isEmpty(querySearch.getRoleIds())) {
            sql.append(" and UR.roleId in :roleIds");
            sql.append(" and UR.deleted = false");
            values.put("roleIds", querySearch.getRoleIds());
        }

        if (!StrUtils.isBlank(querySearch.getAccountType())) {
            sql.append(" and U.accountType = :accountType ");
            values.put("accountType", AccountType.valueOf(querySearch.getAccountType()));
        }

        if (!StrUtils.isBlank(querySearch.getStatus())) {
            sql.append(" and U.status = :status ");
            values.put("status", UserStatus.valueOf(querySearch.getStatus()));
        }
        if (querySearch.isSearchByGroup() == true) {
            sql.append(" and U.id in :userIds ");
            values.put("userIds", querySearch.getUserIds());
        }
        return sql;
    }

    public StringBuilder createOrderQuery(String sortBy) {
        StringBuilder hql = new StringBuilder(" ");
        if (StringUtils.hasLength(sortBy)) {
            hql.append(" order by U.").append(sortBy.replace(".", " "));
        } else {
            hql.append(" order by U.fullName asc ");
        }
        return hql;
    }

    /*
     * Repository for Employee
     */
    @Override
    public List<UserEntity> searchEmployees(EmployeeSearchQuery request) {
        StringBuilder sql = new StringBuilder(("SELECT U FROM UserEntity U "));
        Map<String, Object> values = new HashMap<>();
        sql.append(this.createWhereQueryEmployee(request, values));
        if (Objects.nonNull(request.getSortBy()) && !request.getSortBy().trim().equals("")) {
            sql.append(this.createOrderEmployeeQuery(request));
        }
        Query query = this.entityManager.createQuery(sql.toString(), UserEntity.class);
        values.forEach(query::setParameter);

        query.setFirstResult((request.getPageIndex() - 1) * request.getPageSize());
        query.setMaxResults(request.getPageSize());
        return query.getResultList();
    }

    @Override
    public Long countEmployees(EmployeeSearchQuery request) {
        StringBuilder sql = new StringBuilder("SELECT COUNT(U) FROM UserEntity U ");
        Map<String, Object> values = new HashMap<>();
        sql.append(this.createWhereQueryEmployee(request, values));
        Query query = this.entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);

        return (Long) query.getSingleResult();
    }

    public String createWhereQueryEmployee(EmployeeSearchQuery request, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder(" WHERE 1=1 ");

        if (!StrUtils.isBlank(request.getKeyword())) {
            sql.append("AND ( U.email like :keyword" +
                    " OR U.employeeCode like :keyword" +
                    " OR U.fullName like :keyword" +
                    " OR U.phoneNumber like :keyword )");
            values.put("keyword", SqlUtils.encodeKeyword(request.getKeyword()));

        }
        if (request.getAccountType() != null) {
            sql.append("AND U.accountType=:accountType ");
            values.put("accountType", request.getAccountType());
        }

        if (request.getStatus() != null) {
            sql.append(" and U.status = :status ");
            values.put("status", request.getStatus());
        }

        if (!CollectionUtils.isEmpty(request.getDepartmentIds())) {
            sql.append(" and U.departmentId in :departmentIds ");
            values.put("departmentIds", request.getDepartmentIds());
        }
        sql.append("AND U.deleted = false");
        return sql.toString();
    }

    public StringBuilder createOrderEmployeeQuery(EmployeeSearchQuery employeeSearchRequest) {
        StringBuilder sql = new StringBuilder(" ");
        if (StringUtils.hasLength(employeeSearchRequest.getSortBy())) {
            sql.append(" order by U.").append(employeeSearchRequest.getSortBy().replace(".", " "));
        }
        return sql;
    }
}
