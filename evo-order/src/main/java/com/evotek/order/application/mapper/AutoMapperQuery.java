package com.evotek.order.application.mapper;

import com.evotek.order.application.dto.request.MenuSearchRequest;
import com.evotek.order.application.dto.request.ProductSearchRequest;
import com.evotek.order.application.dto.request.PurchaseOrderSearchRequest;
import com.evotek.order.infrastructure.query.MenuSearchQuery;
import com.evotek.order.infrastructure.query.ProductSearchQuery;
import com.evotek.order.infrastructure.query.PurchaseOrderSearchQuery;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface AutoMapperQuery {
    ProductSearchQuery toQuery(ProductSearchRequest request);

    MenuSearchQuery toQuery(MenuSearchRequest request);

    PurchaseOrderSearchQuery toQuery(PurchaseOrderSearchRequest request);
}
