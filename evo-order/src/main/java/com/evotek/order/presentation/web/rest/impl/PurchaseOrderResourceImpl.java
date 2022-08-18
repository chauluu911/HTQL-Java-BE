package com.evotek.order.presentation.web.rest.impl;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.order.application.dto.request.PurchaseOrderChangeStatusRequest;
import com.evotek.order.application.dto.request.PurchaseOrderCreateRequest;
import com.evotek.order.application.dto.request.PurchaseOrderSearchRequest;
import com.evotek.order.application.dto.request.PurchaseOrderUpdateRequest;
import com.evotek.order.application.service.PurchaseOrderService;
import com.evotek.order.domain.PurchaseOrder;
import com.evotek.order.presentation.web.rest.PurchaseOrderResource;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class PurchaseOrderResourceImpl implements PurchaseOrderResource {

    private final PurchaseOrderService purchaseOrderService;

    public PurchaseOrderResourceImpl(PurchaseOrderService purchaseOrderService) {
        this.purchaseOrderService = purchaseOrderService;
    }

    @Override
    public Response<PurchaseOrder> createPurchaseOrder(PurchaseOrderCreateRequest request) {
        PurchaseOrder purchaseOrder = purchaseOrderService.createPurchaseOrder(request);
        return Response.of(purchaseOrder);
    }

    @Override
    public Response<PurchaseOrder> updatePurchaseOrder(String id, PurchaseOrderUpdateRequest request) {
        PurchaseOrder purchaseOrder = purchaseOrderService.updatePurchaseOrder(id, request);
        return Response.of(purchaseOrder);
    }

    @Override
    public Response<Void> deletePurchaseOrder(String id) {
        purchaseOrderService.deletePurchaseOrderById(id);
        return Response.ok();
    }

    @Override
    public Response<PurchaseOrder> getDetailPurchaseOrder(String id) {
        PurchaseOrder purchaseOrder = purchaseOrderService.getPurchaseOrderById(id);
        return Response.of(purchaseOrder);
    }

    @Override
    public PagingResponse<PurchaseOrder> searchPurchaseOrder(PurchaseOrderSearchRequest request) {
        return PagingResponse.of(purchaseOrderService.search(request));
    }

    @Override
    public Response<Void> acceptOrderStatus(PurchaseOrderChangeStatusRequest request) {
        this.purchaseOrderService.acceptOrderStatusByIds(request);
        return Response.ok();
    }

    @Override
    public Response<Void> acceptDeliveredStatus(PurchaseOrderChangeStatusRequest request) {
        this.purchaseOrderService.acceptDeliveredStatusByIds(request);
        return Response.ok();
    }

    @Override
    public Response<Void> acceptPaymentStatus(PurchaseOrderChangeStatusRequest request) {
        this.purchaseOrderService.acceptPaymentStatusByIds(request);
        return Response.ok();
    }

    @Override
    public Response<Void> revertOrderStatus(PurchaseOrderChangeStatusRequest request) {
        this.purchaseOrderService.revertOrderStatusByIds(request);
        return Response.ok();
    }

}
