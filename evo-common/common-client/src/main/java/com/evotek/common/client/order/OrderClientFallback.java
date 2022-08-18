package com.evotek.common.client.order;

import com.evotek.common.dto.request.LogoutRevokeRequest;
import com.evotek.common.dto.request.PagingRequest;
import com.evotek.common.dto.request.ProductSearchRequest;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.order.ProductDTO;
import com.evotek.common.error.ServiceUnavailableError;
import com.evotek.common.exception.ForwardInnerAlertException;
import com.evotek.common.exception.ResponseException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cloud.openfeign.FallbackFactory;
import org.springframework.stereotype.Component;

@Component
public class OrderClientFallback implements FallbackFactory<OrderClient> {
    @Override
    public OrderClient create(Throwable cause) {
        return new FallbackWithFactory(cause);
    }

    @Slf4j
    static class FallbackWithFactory implements OrderClient {
        private final Throwable cause;

        FallbackWithFactory(Throwable cause) {
            this.cause = cause;
        }


        @Override
        public PagingResponse<ProductDTO> autoComplete(ProductSearchRequest request) {
            log.error("Search all product {} error", request, cause);
            if (cause instanceof ForwardInnerAlertException) {
                return PagingResponse.failPaging((RuntimeException) cause);
            }
            return PagingResponse.failPaging(new ResponseException(ServiceUnavailableError.PRODUCT_SERVICE_UNAVAILABLE_ERROR));
        }

        @Override
        public Response<ProductDTO> findById(String id) {
            log.error("Get product by id {} error", id, cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.PRODUCT_SERVICE_UNAVAILABLE_ERROR));
        }
    }
}
