package com.evotek.order.application.service;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.webapp.support.DomainService;
import com.evotek.order.application.dto.request.MenuCreateRequest;
import com.evotek.order.application.dto.request.MenuSearchRequest;
import com.evotek.order.application.dto.request.MenuUpdateRequest;
import com.evotek.order.domain.Menu;

public interface MenuService extends DomainService<Menu, String> {

    Menu createMenu(MenuCreateRequest request);

    Menu updateMenu(String id, MenuUpdateRequest request);

    void deleteMenuById(String id);

    Menu getDetailMenuById(String id);

    PageDTO<Menu> search(MenuSearchRequest request);

    void publishMenuById(String menuId);

    void unPublishMenuById(String menuId);

    void commitMenuById(String menuId);
}
