package com.evotek.order.infrastructure.repository;

import com.evotek.order.infrastructure.entity.ProductEntity;
import com.evotek.order.infrastructure.repository.custom.ProductRepositoryCustom;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface ProductRepository extends JpaRepository<ProductEntity, String>, ProductRepositoryCustom {

    @NotNull
    @Query("select P from ProductEntity P where P.id = :id and P.deleted = false ")
    Optional<ProductEntity> findById(@NotNull String id);

    @Query("select p from ProductEntity p where p.id in :ids and p.deleted = false")
    List<ProductEntity> getAllByIds(List<String> ids);

}
