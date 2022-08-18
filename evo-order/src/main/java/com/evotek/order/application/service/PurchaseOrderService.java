package com.evotek.order.application.service;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.webapp.support.DomainService;
import com.evotek.order.application.dto.request.PurchaseOrderChangeStatusRequest;
import com.evotek.order.application.dto.request.PurchaseOrderCreateRequest;
import com.evotek.order.application.dto.request.PurchaseOrderSearchRequest;
import com.evotek.order.application.dto.request.PurchaseOrderUpdateRequest;
import com.evotek.order.domain.PurchaseOrder;

public interface PurchaseOrderService extends DomainService<PurchaseOrder, String> {

    PurchaseOrder createPurchaseOrder(PurchaseOrderCreateRequest request);

    PurchaseOrder updatePurchaseOrder(String id, PurchaseOrderUpdateRequest request);

    void deletePurchaseOrderById(String id);

    PurchaseOrder getPurchaseOrderById(String id);

    PageDTO<PurchaseOrder> search(PurchaseOrderSearchRequest request);

    void acceptOrderStatusByIds(PurchaseOrderChangeStatusRequest request);

    void acceptDeliveredStatusByIds(PurchaseOrderChangeStatusRequest request);

    void acceptPaymentStatusByIds(PurchaseOrderChangeStatusRequest request);

    void revertOrderStatusByIds(PurchaseOrderChangeStatusRequest request);


}
