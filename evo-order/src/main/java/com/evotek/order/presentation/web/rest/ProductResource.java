package com.evotek.order.presentation.web.rest;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.order.application.dto.request.ProductCreateRequest;
import com.evotek.order.application.dto.request.ProductSearchRequest;
import com.evotek.order.application.dto.request.ProductUpdateRequest;
import com.evotek.order.domain.Product;
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

@Api(tags = "Product Resource")
@RequestMapping("/api/products")
@Validated
public interface ProductResource {

    @ApiOperation(value = "Create product")
    @PostMapping("")
    @PreAuthorize("hasPermission(null , 'order:create')")
    Response<Product> create(@RequestBody @Valid ProductCreateRequest request);

    @ApiOperation(value = "Update a product lunch")
    @PostMapping("/{id}/update")
    @PreAuthorize("hasPermission(null , 'order:update')")
    Response<Product> update(@PathVariable String id, @RequestBody @Valid ProductUpdateRequest request);


    @ApiOperation(value = "Delete a product")
    @PostMapping("/{id}/delete")
    @PreAuthorize("hasPermission(null , 'order:update')")
    Response<Void> delete(@PathVariable String id);

    @ApiOperation(value = "Search product")
    @GetMapping("")
    @PreAuthorize("hasPermission(null , 'order:view')")
    PagingResponse<Product> search(@ValidatePaging(allowedSorts = {"name", "price", "type"})
                                           ProductSearchRequest request);

}
