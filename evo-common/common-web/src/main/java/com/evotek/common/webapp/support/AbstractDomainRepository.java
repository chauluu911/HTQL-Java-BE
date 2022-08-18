package com.evotek.common.webapp.support;

import com.evotek.common.mapper.EntityMapper;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public abstract class AbstractDomainRepository<D, E, I> implements DomainRepository<D, I> {

    protected final JpaRepository<E, I> jpaRepository;
    protected final EntityMapper<D, E> mapper;

    protected AbstractDomainRepository(JpaRepository<E, I> jpaRepository, EntityMapper<D, E> mapper) {
        this.jpaRepository = jpaRepository;
        this.mapper = mapper;
    }

    @Override
    public Optional<D> findById(I id) {
        return this.jpaRepository.findById(id).map(this.mapper::toDomain).map(this::enrich);
    }

    @Override
    public List<D> findAllByIds(List<I> ids) {
        return this.enrichList(
                this.jpaRepository.findAllById(ids).stream()
                        .map(this.mapper::toDomain).collect(Collectors.toList())
        );
    }

    @Override
    @Transactional
    public D save(D domain) {
        E entity = this.mapper.toEntity(domain);
        this.jpaRepository.save(entity);
        return domain;
    }

    @Override
    @Transactional
    public List<D> saveAll(List<D> domains) {
        List<E> entities = this.mapper.toEntity(domains);
        this.jpaRepository.saveAll(entities);
        return domains;
    }

    protected D enrich(D d) {
        List<D> ds = List.of(d);
        return this.enrichList(ds).get(0);
    }

    protected List<D> enrichList(List<D> ds) {
        return ds;
    }
}