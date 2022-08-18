package com.evotek.order.application.service;

import com.evotek.common.dto.PageDTO;
import com.evotek.order.application.dto.request.ProductCreateRequest;
import com.evotek.order.application.dto.request.ProductSearchRequest;
import com.evotek.order.application.dto.request.ProductUpdateRequest;
import com.evotek.order.domain.Product;

public interface ProductService {
    Product createProduct(ProductCreateRequest request);

    Product updateProduct(String id, ProductUpdateRequest request);

    void deleteProductById(String id);

    Product getDetailProductById(String id);

    PageDTO<Product> search(ProductSearchRequest request);
}
