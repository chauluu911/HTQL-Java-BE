package com.evotek.storage.infrastructure.persistence.repository.impl;

import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.storage.infrastructure.persistence.entity.FileEntity;
import com.evotek.storage.domain.query.FileSearchQuery;
import com.evotek.storage.infrastructure.persistence.repository.custom.FileEntityRepositoryCustom;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Slf4j
public class FileEntityRepositoryImpl implements FileEntityRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<FileEntity> search(FileSearchQuery searchRequest) {
        StringBuilder sql = new StringBuilder();
        Map<String, Object> values = new HashMap<>();
        sql.append("SELECT F FROM FileEntity F ");
        sql.append(this.createWhereQuery(searchRequest, values));
        if (Objects.nonNull(searchRequest.getSortBy()) && !searchRequest.getSortBy().equals("")) {
            sql.append(this.createOrderQuery(searchRequest.getSortBy()));
        }
        Query query = this.entityManager.createQuery(sql.toString(), FileEntity.class);

        values.forEach(query::setParameter);
        query.setFirstResult((searchRequest.getPageIndex() - 1) * searchRequest.getPageSize());
        query.setMaxResults(searchRequest.getPageSize());
        return query.getResultList();
    }

    public Long count(FileSearchQuery searchRequest) {
        try {
            Map<String, Object> values = new HashMap<>();
            String sql = "SELECT COUNT(F) FROM FileEntity F " +
                    this.createWhereQuery(searchRequest, values);
            Query query = this.entityManager.createQuery(sql);
            values.forEach(query::setParameter);
            return (Long) query.getSingleResult();
        } catch (Exception e) {
            log.error(e.getMessage());
        }

        return 0L;
    }

    private String createWhereQuery(FileSearchQuery fileSearchRequest, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder("WHERE F.deleted = false ");
        if (!StrUtils.isBlank(fileSearchRequest.getKeyword())) {
            sql.append("AND (F.originalName LIKE :keyword OR F.path LIKE :keyword OR F.ownerType LIKE :keyword ) ");
            values.put("keyword", SqlUtils.encodeKeyword(fileSearchRequest.getKeyword()));
        }

        if (!StrUtils.isBlank(fileSearchRequest.getOwnerId())) {
            sql.append(" AND F.ownerId = :ownerId ");
            values.put("ownerId", fileSearchRequest.getOwnerId());
        }

        if (!StrUtils.isBlank(fileSearchRequest.getOwnerType())) {
            sql.append(" AND F.ownerType = :ownerType ");
            values.put("ownerType", fileSearchRequest.getOwnerType());
        }
        return sql.toString();
    }

    public StringBuilder createOrderQuery(String sortBy) {
        StringBuilder sql = new StringBuilder(" ");
        if (StringUtils.hasLength(sortBy)) {
            sql.append(" order by F.").append(sortBy.replace("\\.", " "));
        } else {
            sql.append(" order by F.createdAt desc ");
        }
        return sql;
    }
}
