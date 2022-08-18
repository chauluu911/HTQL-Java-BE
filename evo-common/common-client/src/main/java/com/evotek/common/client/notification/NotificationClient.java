package com.evotek.common.client.notification;

import com.evotek.common.dto.request.LogoutRevokeRequest;
import com.evotek.common.dto.request.notification.IssueEventRequest;
import com.evotek.common.dto.response.Response;
import org.springframework.cloud.client.loadbalancer.LoadBalanced;
import org.springframework.cloud.loadbalancer.annotation.LoadBalancerClient;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@LoadBalancerClient(name = "notification")
@FeignClient(name = "notification", fallbackFactory = NotificationClientFallback.class)
public interface NotificationClient {

    @GetMapping("/api/devices/revoke")
    @LoadBalanced
    Response<Boolean> revokeDevice(@RequestBody LogoutRevokeRequest request);

    @PostMapping("/api/events/issue")
    @LoadBalanced
    Response<Void> issueEvent(@RequestBody IssueEventRequest request);
}

