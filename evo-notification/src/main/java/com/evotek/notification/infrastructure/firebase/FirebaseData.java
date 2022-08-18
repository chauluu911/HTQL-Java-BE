package com.evotek.notification.infrastructure.firebase;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class FirebaseData {
    private String topic;
    private String token;
    private String title;
    private String body;
    private Map<String, String> data;
}
