package com.evotek.order.infrastructure.entity;

import com.evotek.common.entity.AuditableEntity;
import com.evotek.common.validator.ValidateConstraint;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.Hibernate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.math.BigDecimal;
import java.time.Instant;
import java.util.Objects;

@Entity
@Table(name = "bill")
@Getter
@Setter
@ToString
@RequiredArgsConstructor
@AllArgsConstructor
public class BillEntity extends AuditableEntity {

    @Id
    @Column(name = "id", length = ValidateConstraint.LENGTH.ID_MAX_LENGTH, nullable = false)
    private String id;

    @Column(name = "status", nullable = false)
    private Boolean status;

    @Column(name = "start_date", nullable = false)
    private Instant startDate;

    @Column(name = "end_date", nullable = false)
    private Instant endDate;

    @Column(name = "price")
    private BigDecimal price;

    @Column(name = "node")
    private String node;

    @Column(name = "deleted")
    private Boolean deleted;

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || Hibernate.getClass(this) != Hibernate.getClass(obj)) return false;
        BillEntity that = (BillEntity) obj;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }
}
