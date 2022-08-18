package com.evotek.order.presentation.web.rest;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.order.application.dto.request.MenuCreateRequest;
import com.evotek.order.application.dto.request.MenuSearchRequest;
import com.evotek.order.application.dto.request.MenuUpdateRequest;
import com.evotek.order.domain.Menu;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.validation.Valid;

@Api(tags = "Menu Resource")
@RequestMapping("/api/menus")
@Validated
public interface MenuResource {
    @ApiOperation(value = "Create menu")
    @PostMapping("")
    @PreAuthorize("hasPermission(null , 'order:create')")
    Response<Menu> createMenu(@RequestBody @Valid MenuCreateRequest request);

    @ApiOperation(value = "Update menu")
    @PostMapping("/{id}/update")
    @PreAuthorize("hasPermission(null , 'order:update')")
    Response<Menu> updateMenu(@PathVariable String id, @RequestBody @Valid MenuUpdateRequest request);

    @ApiOperation(value = "Delete menu")
    @PostMapping("/{id}/delete")
    @PreAuthorize("hasPermission(null , 'order:update')")
    Response<Void> deleteMenu(@PathVariable String id);

    @ApiOperation(value = "Get all menu")
    @GetMapping("")
    @PreAuthorize("hasPermission(null , 'order:view')")
    PagingResponse<Menu> searchMenu(@ValidatePaging(allowedSorts = {"title", "price", "type", "code", "closedAt", "published"})
                                            MenuSearchRequest request);

    @ApiOperation(value = "Get detail menu")
    @GetMapping("/{id}/detail")
    @PreAuthorize("hasPermission(null , 'order:view')")
    Response<Menu> getDetailMenu(@PathVariable String id);

    @ApiOperation(value = "Publish menu")
    @PostMapping("/{id}/publish")
    @PreAuthorize("hasPermission(null , 'order:update')")
    Response<Void> publishMenu(@PathVariable String id);

    @ApiOperation(value = "UnPublish menu")
    @PostMapping("/{id}/unPublish")
    @PreAuthorize("hasPermission(null , 'order:update')")
    Response<Void> unPublishMenu(@PathVariable String id);

    @ApiOperation(value = "Commit menu")
    @PostMapping("/{id}/commit")
    @PreAuthorize("hasPermission(null, 'order:update')")
    Response<Void> commitMenu(@PathVariable String id);
}


