package com.evotek.common.client.system;

import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.system.ConfigurationDTO;
import com.evotek.common.enums.ConfigurationType;
import org.springframework.cloud.client.loadbalancer.LoadBalanced;
import org.springframework.cloud.loadbalancer.annotation.LoadBalancerClient;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@LoadBalancerClient(name = "system")
@FeignClient(name = "system", fallbackFactory = SystemClientFallback.class)
public interface SystemClient {
    @GetMapping("/api/configurations/find-by-code/{code}")
    @LoadBalanced
    Response<ConfigurationDTO> findByCode(@PathVariable ConfigurationType code);
}
