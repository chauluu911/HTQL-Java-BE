package com.evotek.order.application.service.impl;

import com.evotek.common.client.iam.IAMClient;
import com.evotek.common.dto.PageDTO;
import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.iam.UserDTO;
import com.evotek.common.error.AuthenticationError;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.persistence.support.SeqRepository;
import com.evotek.common.webapp.support.AbstractDomainService;
import com.evotek.common.webapp.support.SecurityUtils;
import com.evotek.order.application.dto.request.PurchaseOrderChangeStatusRequest;
import com.evotek.order.application.dto.request.PurchaseOrderCreateRequest;
import com.evotek.order.application.dto.request.PurchaseOrderSearchRequest;
import com.evotek.order.application.dto.request.PurchaseOrderUpdateRequest;
import com.evotek.order.application.mapper.AutoMapper;
import com.evotek.order.application.mapper.MenuEntityMapper;
import com.evotek.order.application.mapper.PurchaseOrderEntityMapper;
import com.evotek.order.application.mapper.PurchaseOrderItemEntityMapper;
import com.evotek.order.application.mapper.ProductEntityMapper;
import com.evotek.order.application.mapper.MenuProductEntityMapper;
import com.evotek.order.application.mapper.AutoMapperQuery;
import com.evotek.order.application.mapper.PurchaseOrderHistoryEntityMapper;
import com.evotek.order.application.service.PurchaseOrderService;
import com.evotek.order.domain.Menu;
import com.evotek.order.domain.PurchaseOrder;
import com.evotek.order.domain.PurchaseOrderHistory;
import com.evotek.order.domain.PurchaseOrderItem;
import com.evotek.order.domain.MenuProduct;
import com.evotek.order.domain.Product;
import com.evotek.order.domain.command.PurchaseOrderCreateCmd;
import com.evotek.order.domain.command.PurchaseOrderUpdateCmd;
import com.evotek.order.domain.repository.PurchaseOrderDomainRepository;
import com.evotek.order.infrastructure.entity.MenuEntity;
import com.evotek.order.infrastructure.entity.PurchaseOrderEntity;
import com.evotek.order.infrastructure.entity.PurchaseOrderHistoryEntity;
import com.evotek.order.infrastructure.entity.PurchaseOrderItemEntity;
import com.evotek.order.infrastructure.query.PurchaseOrderSearchQuery;
import com.evotek.order.infrastructure.repository.MenuProductRepository;
import com.evotek.order.infrastructure.repository.MenuRepository;
import com.evotek.order.infrastructure.repository.ProductRepository;
import com.evotek.order.infrastructure.repository.PurchaseOrderItemRepository;
import com.evotek.order.infrastructure.repository.PurchaseOrderRepository;
import com.evotek.order.infrastructure.repository.PurchaseOrderHistoryRepository;
import com.evotek.order.infrastructure.support.exception.BadRequestError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

@Service
@Slf4j
public class PurchaseOrderServiceImpl extends AbstractDomainService<PurchaseOrder, PurchaseOrderEntity, String> implements PurchaseOrderService {

    private final PurchaseOrderEntityMapper purchaseOrderEntityMapper;
    private final PurchaseOrderRepository orderRepository;
    private final AutoMapper autoMapper;
    private final PurchaseOrderItemEntityMapper purchaseOrderItemEntityMapper;
    private final PurchaseOrderItemRepository purchaseOrderItemRepository;
    private final MenuRepository menuRepository;
    private final MenuEntityMapper menuEntityMapper;
    private final ProductRepository productRepository;
    private final ProductEntityMapper productEntityMapper;
    private final MenuProductRepository menuProductRepository;
    private final MenuProductEntityMapper menuProductEntityMapper;
    private final SeqRepository seqRepository;
    private final AutoMapperQuery autoMapperQuery;
    private final PurchaseOrderRepository purchaseOrderRepository;
    private final PurchaseOrderHistoryRepository purchaseOrderHistoryRepository;
    private final PurchaseOrderHistoryEntityMapper purchaseOrderHistoryEntityMapper;
    private final IAMClient iamClient;
    private final PurchaseOrderDomainRepository purchaseOrderDomainRepository;

    protected PurchaseOrderServiceImpl(PurchaseOrderEntityMapper purchaseOrderEntityMapper,
                                       PurchaseOrderRepository orderRepository,
                                       AutoMapper autoMapper,
                                       PurchaseOrderItemEntityMapper purchaseOrderItemEntityMapper,
                                       PurchaseOrderItemRepository purchaseOrderItemRepository,
                                       MenuRepository menuRepository,
                                       MenuEntityMapper menuEntityMapper,
                                       ProductRepository productRepository,
                                       ProductEntityMapper productEntityMapper,
                                       MenuProductRepository menuProductRepository,
                                       MenuProductEntityMapper menuProductEntityMapper,
                                       SeqRepository seqRepository,
                                       AutoMapperQuery autoMapperQuery,
                                       PurchaseOrderRepository purchaseOrderRepository,
                                       PurchaseOrderHistoryRepository purchaseOrderHistoryRepository,
                                       PurchaseOrderHistoryEntityMapper purchaseOrderHistoryEntityMapper,
                                       PurchaseOrderDomainRepository purchaseOrderDomainRepository,
                                       IAMClient iamClient) {
        super(orderRepository, purchaseOrderEntityMapper);
        this.purchaseOrderEntityMapper = purchaseOrderEntityMapper;
        this.orderRepository = orderRepository;
        this.autoMapper = autoMapper;
        this.purchaseOrderItemEntityMapper = purchaseOrderItemEntityMapper;
        this.purchaseOrderItemRepository = purchaseOrderItemRepository;
        this.menuRepository = menuRepository;
        this.menuEntityMapper = menuEntityMapper;
        this.productRepository = productRepository;
        this.productEntityMapper = productEntityMapper;
        this.menuProductRepository = menuProductRepository;
        this.menuProductEntityMapper = menuProductEntityMapper;
        this.seqRepository = seqRepository;
        this.autoMapperQuery = autoMapperQuery;
        this.purchaseOrderRepository = purchaseOrderRepository;
        this.purchaseOrderHistoryRepository = purchaseOrderHistoryRepository;
        this.purchaseOrderHistoryEntityMapper = purchaseOrderHistoryEntityMapper;
        this.iamClient = iamClient;
        this.purchaseOrderDomainRepository = purchaseOrderDomainRepository;
    }

    @Override
    @Transactional
    public PurchaseOrder createPurchaseOrder(PurchaseOrderCreateRequest request) {
        // enrich menu
        Menu menu = ensureMenuExistedById(request.getMenuId());
        enrichMenu(menu);

        PurchaseOrderCreateCmd cmd = autoMapper.from(request);
        cmd.enrichOwnerId(getUserLoginId());
        if (Boolean.FALSE.equals(cmd.getOwnerId().equals(cmd.getCreatedUserId()))) {
            ensureUserExisted(cmd.getCreatedUserId());
        }
        cmd.enrichCode(seqRepository.generateOrderCode());

        PurchaseOrder purchaseOrder = menu.createPurchaseOrder(cmd);
        // Save purchaseOrder
        this.save(purchaseOrder);
        return purchaseOrder;
    }

    @Override
    @Transactional
    public PurchaseOrder updatePurchaseOrder(String id, PurchaseOrderUpdateRequest request) {
        // Check orderId and get PurchaseOrder
        PurchaseOrder purchaseOrder = ensureOrderExistedById(id);
        enrichPurchaseOrder(purchaseOrder);

        // enrich menu
        Menu menu = ensureMenuExistedById(purchaseOrder.getMenuId());
        enrichMenu(menu);

        //update purchaseOrder
        PurchaseOrderUpdateCmd cmd = autoMapper.from(request);
        purchaseOrder.updatePurchaseOrder(cmd, menu);
        //save
        save(purchaseOrder);
        return purchaseOrder;
    }

    @Override
    @Transactional
    public void deletePurchaseOrderById(String id) {
        // Check orderId and get PurchaseOrder
        PurchaseOrder purchaseOrder = ensureOrderExistedById(id);
        enrichPurchaseOrder(purchaseOrder);
        // delete purchaseOrder
        purchaseOrder.delete();
        save(purchaseOrder);
    }

    @Override
    public PurchaseOrder getPurchaseOrderById(String id) {
        PurchaseOrder purchaseOrder = ensureOrderExistedById(id);
        enrichPurchaseOrder(purchaseOrder);
        return purchaseOrder;
    }

    @Override
    public PageDTO<PurchaseOrder> search(PurchaseOrderSearchRequest request) {
        PurchaseOrderSearchQuery searchQuery = autoMapperQuery.toQuery(request);
        if (Boolean.FALSE.equals(SecurityUtils.isAdmin())) {
            searchQuery.setCreatedUserId(getUserLoginId());
        }
        List<PurchaseOrderEntity> orderEntities = orderRepository.search(searchQuery);
        List<PurchaseOrder> purchaseOrders = purchaseOrderEntityMapper.toDomain(orderEntities);
        purchaseOrders.forEach(purchaseOrder -> enrichPurchaseOrder(purchaseOrder));
        return new PageDTO<>(purchaseOrders,
                request.getPageIndex(),
                request.getPageSize(),
                orderRepository.countOrder(searchQuery));
    }

    @Override
    @Transactional
    public void acceptOrderStatusByIds(PurchaseOrderChangeStatusRequest request) {
        List<PurchaseOrder> purchaseOrders = getPurchaseOrderByIds(request.getPurchaseOrderIds());
        if (CollectionUtils.isEmpty(purchaseOrders)) {
            throw new ResponseException(BadRequestError.PURCHASE_ORDER_IS_EMPTY_TO_CHANGE_STATUS);
        }
        purchaseOrders.forEach(purchaseOrder -> purchaseOrder.acceptOrderStatus(getUserLoginId()));
        this.purchaseOrderDomainRepository.saveAll(purchaseOrders);
    }

    @Override
    @Transactional
    public void acceptDeliveredStatusByIds(PurchaseOrderChangeStatusRequest request) {
        List<PurchaseOrder> purchaseOrders = getPurchaseOrderByIds(request.getPurchaseOrderIds());
        if (CollectionUtils.isEmpty(purchaseOrders)) {
            throw new ResponseException(BadRequestError.PURCHASE_ORDER_IS_EMPTY_TO_CHANGE_STATUS);
        }
        purchaseOrders.forEach(purchaseOrder -> purchaseOrder.acceptDeliveredStatus(getUserLoginId()));
        this.purchaseOrderDomainRepository.saveAll(purchaseOrders);
    }

    @Override
    @Transactional
    public void acceptPaymentStatusByIds(PurchaseOrderChangeStatusRequest request) {
        List<PurchaseOrder> purchaseOrders = getPurchaseOrderByIds(request.getPurchaseOrderIds());
        if (CollectionUtils.isEmpty(purchaseOrders)) {
            throw new ResponseException(BadRequestError.PURCHASE_ORDER_IS_EMPTY_TO_CHANGE_STATUS);
        }
        purchaseOrders.forEach(purchaseOrder -> purchaseOrder.acceptPaymentStatus(getUserLoginId()));
        this.purchaseOrderDomainRepository.saveAll(purchaseOrders);
    }

    @Override
    @Transactional
    public void revertOrderStatusByIds(PurchaseOrderChangeStatusRequest request) {
        List<PurchaseOrder> purchaseOrders = getPurchaseOrderByIds(request.getPurchaseOrderIds());
        if (CollectionUtils.isEmpty(purchaseOrders)) {
            throw new ResponseException(BadRequestError.PURCHASE_ORDER_IS_EMPTY_TO_CHANGE_STATUS);
        }
        purchaseOrders.forEach(purchaseOrder -> purchaseOrder.revertOrderStatus(getUserLoginId()));
        this.purchaseOrderDomainRepository.saveAll(purchaseOrders);
    }

    private List<PurchaseOrder> getPurchaseOrderByIds(List<String> purchaseOrderIds) {
        if (CollectionUtils.isEmpty(purchaseOrderIds)) {
            return new ArrayList<>();
        }
        List<PurchaseOrderEntity> purchaseOrderEntities = purchaseOrderRepository.findByIds(purchaseOrderIds);
        List<PurchaseOrder> purchaseOrders = purchaseOrderEntityMapper.toDomain(purchaseOrderEntities);
        purchaseOrderIds.forEach(id -> {
            if (Boolean.TRUE.equals(purchaseOrders.stream().noneMatch(purchaseOrder -> purchaseOrder.getId().equals(id)))) {
                throw new ResponseException(BadRequestError.PURCHASE_ORDER_NOT_FOUND);
            }
        });
        return purchaseOrders;
    }

    private Menu ensureMenuExistedById(String menuId) {
        MenuEntity menuEntity = menuRepository.findById(menuId)
                .orElseThrow(() -> new ResponseException(BadRequestError.MENU_NOT_FOUND));
        return menuEntityMapper.toDomain(menuEntity);
    }

    private PurchaseOrder ensureOrderExistedById(String orderId) {
        PurchaseOrderEntity purchaseOrderEntity = orderRepository.findById(orderId)
                .orElseThrow(() -> new ResponseException(BadRequestError.PURCHASE_ORDER_NOT_FOUND));
        return purchaseOrderEntityMapper.toDomain(purchaseOrderEntity);
    }

    private void enrichPurchaseOrder(PurchaseOrder purchaseOrder) {
        // enrich purchaseOrder item
        List<PurchaseOrderItemEntity> purchaseOrderItemEntities = purchaseOrderItemRepository.findAllByOrderId(purchaseOrder.getId());
        List<PurchaseOrderItem> purchaseOrderItems = purchaseOrderItemEntityMapper.toDomain(purchaseOrderItemEntities);
        purchaseOrder.enrichPurchaseOrderItems(purchaseOrderItems);
        // enrich purchaseOrder history
        List<PurchaseOrderHistoryEntity> purchaseOrderHistoryEntities = purchaseOrderHistoryRepository.findAllByPurchaseOrderId(purchaseOrder.getId());
        List<PurchaseOrderHistory> purchaseOrderHistories = purchaseOrderHistoryEntityMapper.toDomain(purchaseOrderHistoryEntities);
        purchaseOrderHistories.forEach(purchaseOrderHistory -> purchaseOrderHistory.enrichCreatedUserFullName(getUserNameById(purchaseOrderHistory.getCreatedUserId())));
        purchaseOrder.enrichHistory(purchaseOrderHistories);
        // enrich user info
        purchaseOrder.enrichOwnerFullName(getUserNameById(purchaseOrder.getOwnerId()));
        purchaseOrder.enrichCreatedUserFullName(getUserNameById(purchaseOrder.getCreatedUserId()));
    }

    private void enrichMenu(Menu menu) {
        List<MenuProduct> menuProducts = menuProductEntityMapper.toDomain(menuProductRepository.findMenuProductByMenuId(menu.getId()));
        menu.enrichMenuProduct(menuProducts);
        List<String> productIds = menuProducts.stream().map(MenuProduct::getProductId).collect(Collectors.toList());
        List<Product> products = productEntityMapper.toDomain(productRepository.getAllByIds(productIds));
        menu.enrichProducts(products);
    }

    private String getUserLoginId() {
        String userId = SecurityUtils.getCurrentUserLoginId()
                .orElseThrow(() -> new ResponseException(AuthenticationError.UNAUTHORISED));
        return userId;
    }

    private String getUserNameById(String userId) {
        Response<List<UserDTO>> response = iamClient.findByUserIds(new FindByIdsRequest(Collections.singletonList(userId)));
        if (!response.isSuccess()) {
            throw new ResponseException(BadRequestError.USER_NOT_FOUND);
        }
        UserDTO userDTO = response.getData().get(0);
        return userDTO.getFullName();
    }

    private void ensureUserExisted(String userId) {
        Response<List<UserDTO>> response = iamClient.findByUserIds(new FindByIdsRequest(new ArrayList<>(Collections.singleton(userId))));
        if (!response.isSuccess() || CollectionUtils.isEmpty(response.getData())) {
            throw new ResponseException(BadRequestError.USER_NOT_FOUND);
        }
    }

    @Override
    @Transactional
    public PurchaseOrder save(PurchaseOrder purchaseOrder) {
        PurchaseOrderEntity purchaseOrderEntity = purchaseOrderEntityMapper.toEntity(purchaseOrder);
        orderRepository.save(purchaseOrderEntity);
        if (!CollectionUtils.isEmpty(purchaseOrder.getPurchaseOrderItems())) {
            purchaseOrderItemRepository
                    .saveAll(purchaseOrderItemEntityMapper.toEntity(purchaseOrder.getPurchaseOrderItems()));
        }
        if (!CollectionUtils.isEmpty(purchaseOrder.getPurchaseOrderHistories())) {
            purchaseOrderHistoryRepository
                    .saveAll(purchaseOrderHistoryEntityMapper.toEntity(purchaseOrder.getPurchaseOrderHistories()));
        }
        return purchaseOrder;
    }

}
