package com.evotek.order.infrastructure.repository;

import com.evotek.order.infrastructure.entity.MenuProductEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MenuProductRepository extends JpaRepository<MenuProductEntity, String> {

    @Query("SELECT mp FROM MenuProductEntity mp where mp.menuId = :menuId and mp.deleted = false ")
    List<MenuProductEntity> findMenuProductByMenuId(String menuId);

    @Query("SELECT mp FROM MenuProductEntity mp where mp.productId = :productId and mp.deleted = false ")
    List<MenuProductEntity> findMenuProductByProductId(String productId);
}
