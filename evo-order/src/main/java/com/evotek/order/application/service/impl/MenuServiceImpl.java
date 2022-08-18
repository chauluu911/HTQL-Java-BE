package com.evotek.order.application.service.impl;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.error.AuthenticationError;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.persistence.support.SeqRepository;
import com.evotek.common.webapp.support.AbstractDomainService;
import com.evotek.common.webapp.support.SecurityUtils;
import com.evotek.order.application.dto.request.MenuCreateRequest;
import com.evotek.order.application.dto.request.MenuSearchRequest;
import com.evotek.order.application.dto.request.MenuUpdateRequest;
import com.evotek.order.application.mapper.AutoMapper;
import com.evotek.order.application.mapper.AutoMapperQuery;
import com.evotek.order.application.mapper.MenuEntityMapper;
import com.evotek.order.application.mapper.MenuProductEntityMapper;
import com.evotek.order.application.mapper.ProductEntityMapper;
import com.evotek.order.application.mapper.PurchaseOrderEntityMapper;
import com.evotek.order.application.service.MenuService;
import com.evotek.order.domain.Menu;
import com.evotek.order.domain.MenuProduct;
import com.evotek.order.domain.Product;
import com.evotek.order.domain.PurchaseOrder;
import com.evotek.order.domain.command.MenuCreateCmd;
import com.evotek.order.domain.command.MenuProductCmd;
import com.evotek.order.domain.command.MenuUpdateCmd;
import com.evotek.order.domain.repository.PurchaseOrderDomainRepository;
import com.evotek.order.infrastructure.entity.MenuEntity;
import com.evotek.order.infrastructure.entity.MenuProductEntity;
import com.evotek.order.infrastructure.entity.PurchaseOrderEntity;
import com.evotek.order.infrastructure.query.MenuSearchQuery;
import com.evotek.order.infrastructure.repository.MenuProductRepository;
import com.evotek.order.infrastructure.repository.MenuRepository;
import com.evotek.order.infrastructure.repository.ProductRepository;
import com.evotek.order.infrastructure.repository.PurchaseOrderRepository;
import com.evotek.order.infrastructure.support.exception.BadRequestError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.transaction.Transactional;
import java.util.List;
import java.util.stream.Collectors;

@Service
@Slf4j
public class MenuServiceImpl extends AbstractDomainService<Menu, MenuEntity, String> implements MenuService {

    private final MenuEntityMapper menuEntityMapper;
    private final MenuRepository menuRepository;
    private final AutoMapper autoMapper;
    private final ProductRepository productRepository;
    private final ProductEntityMapper productEntityMapper;
    private final AutoMapperQuery autoMapperQuery;
    private final PurchaseOrderRepository orderRepository;
    private final SeqRepository seqRepository;
    private final PurchaseOrderEntityMapper purchaseOrderEntityMapper;
    private final MenuProductRepository menuProductRepository;
    private final MenuProductEntityMapper menuProductEntityMapper;
    private final PurchaseOrderDomainRepository purchaseOrderDomainRepository;

    public MenuServiceImpl(MenuEntityMapper menuEntityMapper,
                           MenuRepository menuRepository,
                           AutoMapper autoMapper,
                           ProductRepository productRepository,
                           ProductEntityMapper productEntityMapper,
                           AutoMapperQuery autoMapperQuery,
                           PurchaseOrderRepository orderRepository,
                           SeqRepository seqRepository,
                           PurchaseOrderEntityMapper purchaseOrderEntityMapper,
                           MenuProductRepository menuProductRepository,
                           MenuProductEntityMapper menuProductEntityMapper,
                           PurchaseOrderDomainRepository purchaseOrderDomainRepository) {
        super(menuRepository, menuEntityMapper);
        this.menuEntityMapper = menuEntityMapper;
        this.menuRepository = menuRepository;
        this.autoMapper = autoMapper;
        this.autoMapperQuery = autoMapperQuery;
        this.productRepository = productRepository;
        this.productEntityMapper = productEntityMapper;
        this.orderRepository = orderRepository;
        this.seqRepository = seqRepository;
        this.purchaseOrderEntityMapper = purchaseOrderEntityMapper;
        this.menuProductRepository = menuProductRepository;
        this.menuProductEntityMapper = menuProductEntityMapper;
        this.purchaseOrderDomainRepository = purchaseOrderDomainRepository;
    }

    @Override
    @Transactional
    public Menu createMenu(MenuCreateRequest request) {
        MenuCreateCmd cmd = autoMapper.from(request);
        cmd.enrichCode(seqRepository.generateMenuCode());
        List<String> productIds = cmd.getMenuProductCmds().stream().map(MenuProductCmd::getProductId).collect(Collectors.toList());
        List<Product> products = productEntityMapper.toDomain(productRepository.getAllByIds(productIds));
        Menu menu = new Menu(cmd, products);
        save(menu);
        return menu;
    }

    @Override
    @Transactional
    public Menu updateMenu(String id, MenuUpdateRequest request) {
        MenuUpdateCmd cmd = autoMapper.from(request);
        Menu menu = ensureExisted(id);
        enrichMenu(menu);
        List<PurchaseOrder> purchaseOrders = purchaseOrderEntityMapper.toDomain(orderRepository.findByMenuId(id));
        List<String> productIds = cmd.getMenuProductCmds().stream().map(MenuProductCmd::getProductId).collect(Collectors.toList());
        List<Product> products = productEntityMapper.toDomain(productRepository.getAllByIds(productIds));
        menu.updateMenu(cmd, products, purchaseOrders);
        save(menu);
        return menu;
    }

    @Override
    @Transactional
    public void deleteMenuById(String id) {
        Menu menu = ensureExisted(id);
        enrichMenu(menu);
        menu.delete();
        save(menu);
    }

    @Override
    public Menu getDetailMenuById(String id) {
        //1. Check menu id exist or active
        Menu menu = ensureExisted(id);
        // 2. get data
        enrichMenu(menu);
        return menu;
    }

    @Override
    public PageDTO<Menu> search(MenuSearchRequest request) {
        MenuSearchQuery query = autoMapperQuery.toQuery(request);
        List<MenuEntity> menuEntities = menuRepository.search(query);
        List<Menu> menus = menuEntityMapper.toDomain(menuEntities);
        menus.forEach(this::enrichMenu);
        return new PageDTO<>(menus,
                request.getPageIndex(),
                request.getPageSize(),
                menuRepository.countMenu(query));
    }

    @Override
    @Transactional
    public void publishMenuById(String menuId) {
        Menu menu = ensureExisted(menuId);
        menu.publishMenu();
        save(menu);
    }

    @Override
    @Transactional
    public void unPublishMenuById(String menuId) {
        Menu menu = ensureExisted(menuId);
        menu.unPublishMenu();
        save(menu);
    }

    @Override
    @Transactional
    public void commitMenuById(String menuId) {
        Menu menu = ensureExisted(menuId);
        if(menu.getPublished()) {
            menu.unPublishMenu();
        }
        save(menu);

        List<PurchaseOrderEntity> orderEntities = orderRepository.findByMenuId(menuId);
        List<PurchaseOrder> orders = purchaseOrderEntityMapper.toDomain(orderEntities);
        String userLoginId = getUserLoginId();
        orders.forEach(order -> {
            order.acceptOrderStatus(userLoginId);
        });
        this.purchaseOrderDomainRepository.saveAll(orders);
    }

    private String getUserLoginId() {
        String userId = SecurityUtils.getCurrentUserLoginId()
                .orElseThrow(() -> new ResponseException(AuthenticationError.UNAUTHORISED));
        return userId;
    }

    private Menu ensureExisted(String menuId) {
        MenuEntity menuEntity = menuRepository.findById(menuId).orElseThrow(
                () -> new ResponseException(BadRequestError.MENU_NOT_FOUND));
        return menuEntityMapper.toDomain(menuEntity);
    }

    private List<MenuProduct> getMenuProductByMenuId(String menuId) {
        List<MenuProductEntity> menuProductEntities = menuProductRepository.findMenuProductByMenuId(menuId);
        return menuProductEntityMapper.toDomain(menuProductEntities);
    }

    private void enrichMenu(Menu menu) {
        menu.enrichMenuProduct(getMenuProductByMenuId(menu.getId()));
        List<String> productIds = menu.getMenuProducts().stream().map(MenuProduct::getProductId).collect(Collectors.toList());
        List<Product> products = productEntityMapper.toDomain(productRepository.getAllByIds(productIds));
        menu.enrichProducts(products);
    }

    @Override
    @Transactional
    public Menu save(Menu menu) {
        MenuEntity menuEntity = menuEntityMapper.toEntity(menu);
        menuRepository.save(menuEntity);
        if (!CollectionUtils.isEmpty(menu.getMenuProducts())) {
            menuProductRepository.saveAll(menuProductEntityMapper.toEntity(menu.getMenuProducts()));
        }
        return menu;
    }
}
