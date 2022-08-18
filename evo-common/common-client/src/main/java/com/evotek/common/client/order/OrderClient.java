package com.evotek.common.client.order;

import com.evotek.common.dto.request.PagingRequest;
import com.evotek.common.dto.request.ProductSearchRequest;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.order.ProductDTO;
import org.springframework.cloud.client.loadbalancer.LoadBalanced;
import org.springframework.cloud.loadbalancer.annotation.LoadBalancerClient;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.cloud.openfeign.SpringQueryMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@LoadBalancerClient(name = "order")
@FeignClient(name = "order", fallbackFactory = OrderClientFallback.class)
public interface OrderClient {

    @GetMapping("/api/products")
    @LoadBalanced
    PagingResponse<ProductDTO> autoComplete(@SpringQueryMap ProductSearchRequest request);

    @GetMapping("/api/products/{id}")
    @LoadBalanced
    Response<ProductDTO> findById(@PathVariable String id);

}
