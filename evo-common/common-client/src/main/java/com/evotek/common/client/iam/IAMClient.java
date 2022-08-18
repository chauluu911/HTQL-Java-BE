package com.evotek.common.client.iam;

import com.evotek.common.UserAuthority;
import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.request.PagingRequest;
import com.evotek.common.dto.request.iam.ClientLoginRequest;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.iam.ClientToken;
import com.evotek.common.dto.response.iam.DepartmentDTO;
import com.evotek.common.dto.response.iam.UserDTO;
import org.springframework.cloud.client.loadbalancer.LoadBalanced;
import org.springframework.cloud.loadbalancer.annotation.LoadBalancerClient;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.cloud.openfeign.SpringQueryMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import javax.validation.Valid;
import java.util.List;

@LoadBalancerClient(name = "iam")
@FeignClient(name = "iam", fallbackFactory = IAMClientFallback.class)
public interface IAMClient {

    int DEFAULT_PAGE_INDEX = 1;
    int DEFAULT_PAGE_SIZE = 2;

    @GetMapping("/api/users/{userId}/authorities")
    @LoadBalanced
    Response<UserAuthority> getUserAuthority(@PathVariable String userId);

    @GetMapping("/api/clients/me/authorities")
    @LoadBalanced
    Response<UserAuthority> getClientAuthority();

    @PostMapping("/api/users/find-by-ids")
    @LoadBalanced
    Response<List<UserDTO>> findByUserIds(@RequestBody FindByIdsRequest request);

    @GetMapping("/api/users/auto-complete")
    @LoadBalanced
    PagingResponse<UserDTO> autocomplete(@SpringQueryMap PagingRequest request);

    @GetMapping("/api/users/{id}")
    @LoadBalanced
    Response<UserDTO> findUserById(@PathVariable String id);

    default List<UserDTO> getAllUser() {
        int pageIndex = DEFAULT_PAGE_INDEX;
        int pageSize = DEFAULT_PAGE_SIZE;
        PagingResponse<UserDTO> pagingResponse = autocomplete(PagingRequest.builder().pageIndex(pageIndex).pageSize(pageSize).build());
        PagingResponse.PageableResponse pageable = pagingResponse.getPage();
        int totalPage = (int) Math.ceil((double) pageable.getTotal() / pageable.getPageSize());
        List<UserDTO> users = pagingResponse.getData();
        while (pageIndex < totalPage) {
            pagingResponse = autocomplete(PagingRequest.builder().pageIndex(++pageIndex).pageSize(pageSize).build());
            List<UserDTO> userLst = pagingResponse.getData();
            users.addAll(userLst);
        }
        return users;
    }

    @GetMapping("/api/departments")
    @LoadBalanced
    Response<List<DepartmentDTO>> getAllDepartment();

    @PostMapping("/api/departments/find-by-ids")
    @LoadBalanced
    Response<List<DepartmentDTO>> getDepartmentByIds(@RequestBody FindByIdsRequest request);

    @PostMapping("/api/users/find-by-department-ids")
    @LoadBalanced
    Response<List<UserDTO>> findByDepartmentIds(@RequestBody FindByIdsRequest request);

    @PostMapping("/api/client/authenticate")
    @LoadBalanced
    Response<ClientToken> getTokenClient(@RequestBody @Valid ClientLoginRequest request);
}
