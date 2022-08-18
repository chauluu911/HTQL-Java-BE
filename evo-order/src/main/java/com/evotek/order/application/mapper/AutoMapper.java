package com.evotek.order.application.mapper;

import com.evotek.order.application.dto.request.MenuCreateRequest;
import com.evotek.order.application.dto.request.MenuProductRequest;
import com.evotek.order.application.dto.request.MenuUpdateRequest;
import com.evotek.order.application.dto.request.PurchaseOrderItemRequest;
import com.evotek.order.application.dto.request.PurchaseOrderCreateRequest;
import com.evotek.order.application.dto.request.PurchaseOrderUpdateRequest;
import com.evotek.order.application.dto.request.ProductCreateRequest;
import com.evotek.order.application.dto.request.ProductUpdateRequest;
import com.evotek.order.domain.command.MenuCreateCmd;
import com.evotek.order.domain.command.MenuProductCmd;
import com.evotek.order.domain.command.MenuUpdateCmd;
import com.evotek.order.domain.command.PurchaseOrderItemCmd;
import com.evotek.order.domain.command.PurchaseOrderCreateCmd;
import com.evotek.order.domain.command.PurchaseOrderUpdateCmd;
import com.evotek.order.domain.command.ProductCreateCmd;
import com.evotek.order.domain.command.ProductUpdateCmd;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.ArrayList;
import java.util.List;

@Mapper(componentModel = "spring")
public interface AutoMapper {

    @Named("menuProductMapToCmd")
    static List<MenuProductCmd> menuProductMapToCmd(List<MenuProductRequest> menuProductRequests) {
        List<MenuProductCmd> menuProductCmds = new ArrayList<>();
        for (MenuProductRequest request : menuProductRequests) {
            menuProductCmds.add(new MenuProductCmd(request.getProductId(), request.getMaxProductPurchaseOrder()));
        }
        return menuProductCmds;
    }

    @Named("mapToCmd")
    static List<PurchaseOrderItemCmd> mapToCmd(List<PurchaseOrderItemRequest> purchaseOrderItemRequests) {
        List<PurchaseOrderItemCmd> purchaseOrderItemCmds = new ArrayList<>();
        purchaseOrderItemRequests.forEach(oir ->
                purchaseOrderItemCmds.add(new PurchaseOrderItemCmd(oir.getProductId(), oir.getQuantity())));
        return purchaseOrderItemCmds;
    }

    @Mapping(source = "menuProductRequests", target = "menuProductCmds", qualifiedByName = "menuProductMapToCmd")
    MenuCreateCmd from(MenuCreateRequest request);

    @Mapping(source = "menuProductRequests", target = "menuProductCmds", qualifiedByName = "menuProductMapToCmd")
    MenuUpdateCmd from(MenuUpdateRequest request);

    @Mapping(source = "purchaseOrderItems", target = "purchaseOrderItems", qualifiedByName = "mapToCmd")
    PurchaseOrderCreateCmd from(PurchaseOrderCreateRequest request);

    @Mapping(source = "purchaseOrderItems", target = "purchaseOrderItems", qualifiedByName = "mapToCmd")
    PurchaseOrderUpdateCmd from(PurchaseOrderUpdateRequest request);

    PurchaseOrderItemCmd from(PurchaseOrderItemRequest request);

    ProductCreateCmd from(ProductCreateRequest request);

    ProductUpdateCmd from(ProductUpdateRequest request);

    MenuProductCmd from(MenuProductRequest request);
}
