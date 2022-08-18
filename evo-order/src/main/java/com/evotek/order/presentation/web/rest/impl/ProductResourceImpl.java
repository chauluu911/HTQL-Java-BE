package com.evotek.order.presentation.web.rest.impl;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.order.application.dto.request.ProductCreateRequest;
import com.evotek.order.application.dto.request.ProductSearchRequest;
import com.evotek.order.application.dto.request.ProductUpdateRequest;
import com.evotek.order.application.service.ProductService;
import com.evotek.order.domain.Product;
import com.evotek.order.presentation.web.rest.ProductResource;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class ProductResourceImpl implements ProductResource {
    private final ProductService productService;

    public ProductResourceImpl(ProductService productService) {
        this.productService = productService;
    }

    @Override
    public Response<Product> create(ProductCreateRequest request) {
        Product product = productService.createProduct(request);
        return Response.of(product);
    }

    @Override
    public Response<Product> update(String id, ProductUpdateRequest request) {
        Product product = productService.updateProduct(id, request);
        return Response.of(product);
    }

    @Override
    public Response<Void> delete(String id) {
        productService.deleteProductById(id);
        return Response.ok();
    }

    @Override
    public PagingResponse<Product> search(ProductSearchRequest request) {
        return PagingResponse.of(productService.search(request));
    }
}
