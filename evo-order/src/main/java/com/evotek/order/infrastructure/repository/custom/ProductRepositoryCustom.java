package com.evotek.order.infrastructure.repository.custom;

import com.evotek.order.infrastructure.entity.ProductEntity;
import com.evotek.order.infrastructure.query.ProductSearchQuery;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ProductRepositoryCustom {
    List<ProductEntity> search(ProductSearchQuery searchQuery);

    Long countProduct(ProductSearchQuery searchQuery);
}
