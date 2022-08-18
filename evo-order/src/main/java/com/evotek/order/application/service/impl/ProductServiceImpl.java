package com.evotek.order.application.service.impl;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.persistence.support.SeqRepository;
import com.evotek.common.webapp.support.AbstractDomainService;
import com.evotek.order.application.dto.request.ProductCreateRequest;
import com.evotek.order.application.dto.request.ProductSearchRequest;
import com.evotek.order.application.dto.request.ProductUpdateRequest;
import com.evotek.order.application.mapper.AutoMapper;
import com.evotek.order.application.mapper.AutoMapperQuery;
import com.evotek.order.application.mapper.ProductEntityMapper;
import com.evotek.order.application.mapper.MenuProductEntityMapper;
import com.evotek.order.application.service.ProductService;
import com.evotek.order.domain.MenuProduct;
import com.evotek.order.domain.Product;
import com.evotek.order.domain.command.ProductCreateCmd;
import com.evotek.order.domain.command.ProductUpdateCmd;
import com.evotek.order.infrastructure.entity.ProductEntity;
import com.evotek.order.infrastructure.query.ProductSearchQuery;
import com.evotek.order.infrastructure.repository.MenuProductRepository;
import com.evotek.order.infrastructure.repository.ProductRepository;
import com.evotek.order.infrastructure.support.exception.BadRequestError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class ProductServiceImpl extends AbstractDomainService<Product, ProductEntity, String> implements ProductService {
    private final ProductRepository productRepository;
    private final ProductEntityMapper productEntityMapper;
    private final AutoMapper autoMapper;
    private final AutoMapperQuery autoMapperQuery;
    private final SeqRepository seqRepository;
    private final MenuProductRepository menuProductRepository;
    private final MenuProductEntityMapper menuProductEntityMapper;

    public ProductServiceImpl(ProductRepository productRepository,
                              ProductEntityMapper productEntityMapper,
                              AutoMapper autoMapper,
                              AutoMapperQuery autoMapperQuery,
                              SeqRepository seqRepository,
                              MenuProductRepository menuProductRepository,
                              MenuProductEntityMapper menuProductEntityMapper) {
        super(productRepository, productEntityMapper);
        this.productRepository = productRepository;
        this.productEntityMapper = productEntityMapper;
        this.autoMapper = autoMapper;
        this.autoMapperQuery = autoMapperQuery;
        this.seqRepository = seqRepository;
        this.menuProductRepository = menuProductRepository;
        this.menuProductEntityMapper = menuProductEntityMapper;
    }

    @Override
    public Product createProduct(ProductCreateRequest request) {
        ProductCreateCmd productCreateCmd = autoMapper.from(request);
        productCreateCmd.enrichCode(seqRepository.generateProductCode());
        Product product = new Product(productCreateCmd);
        save(product);
        return product;
    }

    @Override
    public Product updateProduct(String id, ProductUpdateRequest request) {
        Product product = ensureExisted(id);
        ProductUpdateCmd productUpdateCmd = autoMapper.from(request);
        product.updateProduct(productUpdateCmd);
        save(product);
        return product;
    }

    @Override
    public void deleteProductById(String id) {
        Product product = ensureExisted(id);
        List<MenuProduct> menuProducts = menuProductEntityMapper.toDomain(menuProductRepository.findMenuProductByProductId(id));
        product.delete(menuProducts);
        save(product);
    }

    @Override
    public Product getDetailProductById(String id) {
        return ensureExisted(id);
    }

    @Override
    public PageDTO<Product> search(ProductSearchRequest request) {
        ProductSearchQuery query = autoMapperQuery.toQuery(request);
        List<ProductEntity> productEntities = productRepository.search(query);
        List<Product> products = productEntityMapper.toDomain(productEntities);
        return new PageDTO<>(products,
                request.getPageIndex(),
                request.getPageSize(),
                productRepository.countProduct(query));
    }

    private Product ensureExisted(String productId) {
        ProductEntity productEntity = productRepository.findById(productId)
                .orElseThrow(() -> new ResponseException(BadRequestError.PRODUCT_NOT_FOUND));
        return productEntityMapper.toDomain(productEntity);
    }
}
