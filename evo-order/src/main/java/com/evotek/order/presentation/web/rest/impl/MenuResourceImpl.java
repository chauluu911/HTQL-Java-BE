package com.evotek.order.presentation.web.rest.impl;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.order.application.dto.request.MenuCreateRequest;
import com.evotek.order.application.dto.request.MenuSearchRequest;
import com.evotek.order.application.dto.request.MenuUpdateRequest;
import com.evotek.order.application.service.MenuService;
import com.evotek.order.domain.Menu;
import com.evotek.order.presentation.web.rest.MenuResource;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class MenuResourceImpl implements MenuResource {

    private final MenuService menuService;

    public MenuResourceImpl(MenuService menuService) {
        this.menuService = menuService;
    }

    @Override
    public Response<Menu> createMenu(MenuCreateRequest request) {
        Menu menu = this.menuService.createMenu(request);
        return Response.of(menu);
    }

    @Override
    public Response<Menu> updateMenu(String id, MenuUpdateRequest request) {
        Menu menu = menuService.updateMenu(id, request);
        return Response.of(menu);
    }

    @Override
    public Response<Void> deleteMenu(String id) {
        this.menuService.deleteMenuById(id);
        return Response.ok();
    }

    @Override
    public PagingResponse<Menu> searchMenu(MenuSearchRequest request) {
        return PagingResponse.of(menuService.search(request));
    }

    @Override
    public Response<Menu> getDetailMenu(String id) {
        Menu menu = menuService.getDetailMenuById(id);
        return Response.of(menu);
    }

    @Override
    public Response<Void> publishMenu(String id) {
        this.menuService.publishMenuById(id);
        return Response.ok();
    }

    @Override
    public Response<Void> unPublishMenu(String id) {
        this.menuService.unPublishMenuById(id);
        return Response.ok();
    }

    @Override
    public  Response<Void> commitMenu(String id) {
        this.menuService.commitMenuById(id);
        return Response.ok();
    }
}
