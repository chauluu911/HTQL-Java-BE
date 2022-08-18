package com.evotek.common.dto.response.order;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.math.BigDecimal;

@Data
@AllArgsConstructor
@Builder
public class ProductDTO implements Serializable {
    private String id;
    private String code;
    private String name;
    private BigDecimal price;
    private String imageId;
    private String type;
    private Boolean deleted;

}
