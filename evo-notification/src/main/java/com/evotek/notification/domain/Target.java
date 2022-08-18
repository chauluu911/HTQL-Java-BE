package com.evotek.notification.domain;

import lombok.Builder;
import lombok.Data;

import java.io.Serializable;

@Data
@Builder
public class Target implements Serializable {
    private String userId;
    private String email;

    public Target(String userId, String email) {
        this.userId = userId;
        this.email = email;
    }
}
