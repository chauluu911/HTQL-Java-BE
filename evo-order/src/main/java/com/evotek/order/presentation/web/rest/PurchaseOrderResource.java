package com.evotek.order.presentation.web.rest;

import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.order.application.dto.request.PurchaseOrderChangeStatusRequest;
import com.evotek.order.application.dto.request.PurchaseOrderCreateRequest;
import com.evotek.order.application.dto.request.PurchaseOrderSearchRequest;
import com.evotek.order.application.dto.request.PurchaseOrderUpdateRequest;
import com.evotek.order.domain.PurchaseOrder;
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

@Api(tags = "PurchaseOrder Resource")
@RequestMapping("/api/purchase-order")
@Validated
public interface PurchaseOrderResource {

    @ApiOperation(value = "Create order")
    @PostMapping("")
    @PreAuthorize("hasPermission(null , 'order:create')")
    Response<PurchaseOrder> createPurchaseOrder(@RequestBody @Valid PurchaseOrderCreateRequest request);

    @ApiOperation(value = "Update order")
    @PostMapping("/{id}/update")
    @PreAuthorize("hasPermission(null , 'order:update')")
    Response<PurchaseOrder> updatePurchaseOrder(@PathVariable String id, @RequestBody @Valid PurchaseOrderUpdateRequest request);

    @ApiOperation(value = "Delete order")
    @PostMapping("/{id}/delete")
    @PreAuthorize("hasPermission(null , 'order:update')")
    Response<Void> deletePurchaseOrder(@PathVariable String id);

    @ApiOperation(value = "Detail order")
    @GetMapping("/{id}/detail")
    @PreAuthorize("hasPermission(null , 'order:view')")
    Response<PurchaseOrder> getDetailPurchaseOrder(@PathVariable String id);

    @ApiOperation(value = "Search order")
    @GetMapping("")
    @PreAuthorize("hasPermission(null , 'order:view')")
    PagingResponse<PurchaseOrder> searchPurchaseOrder(@ValidatePaging(allowedSorts = {"totalPrice", "type", "code", "status"})
                                                      PurchaseOrderSearchRequest request);

    @ApiOperation(value = "change status to ORDER")
    @PostMapping("/change-status-order")
    @PreAuthorize("hasPermission(null , 'order:update')")
    Response<Void> acceptOrderStatus(@RequestBody @Valid PurchaseOrderChangeStatusRequest request);

    @ApiOperation(value = "change status to DELIVERED")
    @PostMapping("/change-status-delivered")
    @PreAuthorize("hasPermission(null , 'order:update')")
    Response<Void> acceptDeliveredStatus(@RequestBody @Valid PurchaseOrderChangeStatusRequest request);

    @ApiOperation(value = "change status to PAID")
    @PostMapping("/change-status-paid")
    @PreAuthorize("hasPermission(null , 'order:update')")
    Response<Void> acceptPaymentStatus(@RequestBody @Valid PurchaseOrderChangeStatusRequest request);

    @ApiOperation(value = "revert status to PAID")
    @PostMapping("/revert-status-order")
    @PreAuthorize("hasPermission(null , 'order:update')")
    Response<Void> revertOrderStatus(@RequestBody @Valid PurchaseOrderChangeStatusRequest request);
}
