package com.evotek.meet.infrastructure.persistence.repository.impl;

import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.meet.infrastructure.persistence.entity.MeetingEntity;
import com.evotek.meet.domain.query.MeetingSearchQuery;
import com.evotek.meet.infrastructure.persistence.repository.custom.MeetingEntityRepositoryCustom;
import lombok.extern.slf4j.Slf4j;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Slf4j
public class MeetingEntityRepositoryImpl implements MeetingEntityRepositoryCustom {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public List<MeetingEntity> search(MeetingSearchQuery searchRequest) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        if (!StrUtils.isBlank(searchRequest.getUserId())) {
            sql.append("SELECT distinct m FROM MeetingEntity m ");
            sql.append("INNER JOIN MeetingAttendeeEntity ma ON m.id = ma.meetingId ");
        } else {
            sql.append("SELECT m FROM MeetingEntity m ");
        }
        sql.append(this.createWhereQuery(searchRequest, values));
        sql.append(this.createOrderQuery(searchRequest));
        Query query = this.entityManager.createQuery(sql.toString(), MeetingEntity.class);
        values.forEach(query::setParameter);
        query.setFirstResult((searchRequest.getPageIndex() - 1) * searchRequest.getPageSize());
        query.setMaxResults(searchRequest.getPageSize());
        return query.getResultList();
    }

    @Override
    public Long count(MeetingSearchQuery searchRequest) {
        Map<String, Object> values = new HashMap<>();
        StringBuilder sql = new StringBuilder();
        if (!StrUtils.isBlank(searchRequest.getUserId())) {
            sql.append("SELECT count(distinct m) FROM MeetingEntity m ");
            sql.append("INNER JOIN MeetingAttendeeEntity ma ON m.id = ma.meetingId ");
        } else {
            sql.append("SELECT count(m) FROM MeetingEntity m ");
        }
        sql.append(this.createWhereQuery(searchRequest, values));
        Query query = this.entityManager.createQuery(sql.toString(), Long.class);
        values.forEach(query::setParameter);
        return (Long) query.getSingleResult();
    }

    public String createWhereQuery(MeetingSearchQuery searchRequest, Map<String, Object> values) {
        StringBuilder sql = new StringBuilder(" WHERE m.deleted = false ");
        if (!StrUtils.isBlank(searchRequest.getUserId())) {
            sql.append(" AND ma.userId = :userId ");
            values.put("userId", searchRequest.getUserId());
        }
        if (!StrUtils.isBlank(searchRequest.getKeyword())) {
            sql.append("AND ( m.title LIKE :keyword ) ");
            values.put("keyword", SqlUtils.encodeKeyword(searchRequest.getKeyword()));
        }
        if (!StrUtils.isBlank(searchRequest.getTitle())) {
            sql.append(" AND m.title LIKE :title ");
            values.put("title", SqlUtils.encodeKeyword(searchRequest.getTitle()));
        }
        if (Objects.nonNull(searchRequest.getRepeatType())) {
            sql.append(" AND m.repeatType = :repeatType ");
            values.put("repeatType", searchRequest.getRepeatType());
        }
        if (Objects.nonNull(searchRequest.getMeetingStatus())) {
            sql.append(" AND m.meetingStatus = :meetingStatus ");
            values.put("meetingStatus", searchRequest.getMeetingStatus());
        }
        if (Objects.nonNull(searchRequest.getRoomId())) {
            sql.append(" AND m.roomId = :roomId ");
            values.put("roomId", searchRequest.getRoomId());
        }
        if (Objects.nonNull(searchRequest.getStartAt())) {
            sql.append(" AND m.startAt >= :startDate ");
            values.put("startDate", searchRequest.getStartAt());
        }
        if (Objects.nonNull(searchRequest.getEndDate())) {
            sql.append(" AND m.endDate <= :endDate ");
            values.put("endDate", searchRequest.getEndDate());
        }
        if (Objects.nonNull(searchRequest.getMeetingType())) {
            sql.append(" AND m.meetingType = :meetingType ");
            values.put("meetingType", searchRequest.getMeetingType());
        }
        return sql.toString();
    }

    public StringBuilder createOrderQuery(MeetingSearchQuery searchRequest) {
        StringBuilder sql = new StringBuilder(" ");
        if (!StrUtils.isBlank(searchRequest.getSortBy())) {
            sql.append(" order by m. ").append(searchRequest.getSortBy().replace(".", " "));
        } else {
            sql.append(" order by m.createdAt desc");
        }
        return sql;
    }


}
