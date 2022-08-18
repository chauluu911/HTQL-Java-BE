package com.evotek.order.application.dto.request;

import com.evotek.common.dto.request.Request;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
public class PurchaseOrderChangeStatusRequest extends Request {
    List<String> purchaseOrderIds;
}
