package com.evotek.order.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.IdUtils;
import com.evotek.order.domain.command.ProductCreateCmd;
import com.evotek.order.domain.command.ProductUpdateCmd;
import com.evotek.order.infrastructure.support.enums.ProductSubType;
import com.evotek.order.infrastructure.support.enums.ProductType;
import com.evotek.order.infrastructure.support.exception.BadRequestError;
import lombok.EqualsAndHashCode;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.List;
import java.util.Objects;

@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
@Setter
@Getter
@SuperBuilder
public class Product extends AuditableDomain {
    private String id;
    private String code;
    private String name;
    private BigDecimal price;
    private ProductType type;
    private ProductSubType subType;
    private String imageId;
    private Boolean deleted;

    public Product(ProductCreateCmd cmd) {
        this.id = IdUtils.nextId();
        this.code = cmd.getCode();
        this.name = cmd.getName();
        this.type = cmd.getType();
        this.imageId = cmd.getImageId();
        enrichPrice(cmd.getPrice());
        enrichSubType(cmd.getSubType());
        this.deleted = Boolean.FALSE;
    }

    public void updateProduct(ProductUpdateCmd cmd) {
        this.name = cmd.getName();
        this.type = cmd.getType();
        this.imageId = cmd.getImageId();
        enrichPrice(cmd.getPrice());
        enrichSubType(cmd.getSubType());
    }

    public void delete(List<MenuProduct> menuProducts) {
        // check product not used
        if (!CollectionUtils.isEmpty(menuProducts)) {
            throw new ResponseException(BadRequestError.MENU_USING_PRODUCT);
        }
        this.deleted = Boolean.TRUE;
    }

    private void enrichPrice(BigDecimal price) {
        if (ProductType.LUNCH.equals(this.type)) {
            this.price = BigDecimal.ZERO;
        } else {
            if (!Objects.nonNull(price)) {
                throw new ResponseException(BadRequestError.PRODUCT_MUST_HAVE_PRICE);
            }
            this.price = price;
        }
    }

    private void enrichSubType(ProductSubType subType) {
        if (ProductType.LUNCH.equals(this.type)) {
            if (!Objects.nonNull(subType)) {
                throw new ResponseException(BadRequestError.PRODUCT_MUST_HAVE_SUBTYPE);
            }
            this.subType = subType;
        } else {
            this.subType = null;
        }
    }
}
