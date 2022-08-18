package com.evotek.common.webapp.support;

import java.util.List;
import java.util.Optional;

public interface DomainRepository<D, I> {

    Optional<D> findById(I id);

    List<D> findAllByIds(List<I> ids);

    D save(D domain);

    List<D> saveAll(List<D> domains);

    D getById(I id);
}
