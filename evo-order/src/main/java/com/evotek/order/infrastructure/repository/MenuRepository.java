package com.evotek.order.infrastructure.repository;

import com.evotek.order.infrastructure.entity.MenuEntity;
import com.evotek.order.infrastructure.repository.custom.MenuRepositoryCustom;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface MenuRepository extends JpaRepository<MenuEntity, String>, MenuRepositoryCustom {

    @NotNull
    @Query("select m from MenuEntity m where m.id = :id and m.deleted = false ")
    Optional<MenuEntity> findById(@NotNull String id);
}
