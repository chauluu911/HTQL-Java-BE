package com.evotek.order.infrastructure.repository.custom;

import com.evotek.order.infrastructure.entity.MenuEntity;
import com.evotek.order.infrastructure.query.MenuSearchQuery;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MenuRepositoryCustom {

    List<MenuEntity> search(MenuSearchQuery searchQuery);

    Long countMenu(MenuSearchQuery searchQuery);
}
