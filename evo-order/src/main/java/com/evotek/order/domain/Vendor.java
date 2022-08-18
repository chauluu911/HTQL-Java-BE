package com.evotek.order.domain;

import com.evotek.common.domain.AuditableDomain;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.Getter;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;

@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
@Setter
@Getter
@SuperBuilder
public class Vendor extends AuditableDomain {
    private String id;
    private String name;
    private String phone;
    private String email;
    private String address;
    private String bankName;
    private String bankNumber;
    private BigDecimal price;
    private Integer numberOfSaltyFood;
    private Integer numberOfBalendFood;
}
