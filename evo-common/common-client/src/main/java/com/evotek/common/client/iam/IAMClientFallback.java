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
import com.evotek.common.error.ServiceUnavailableError;
import com.evotek.common.exception.ForwardInnerAlertException;
import com.evotek.common.exception.ResponseException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cloud.openfeign.FallbackFactory;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class IAMClientFallback implements FallbackFactory<IAMClient> {

    @Override
    public IAMClient create(Throwable cause) {
        return new FallbackWithFactory(cause);
    }

    @Slf4j
    static class FallbackWithFactory implements IAMClient {

        private final Throwable cause;

        FallbackWithFactory(Throwable cause) {
            this.cause = cause;
        }

        @Override
        public Response<UserAuthority> getUserAuthority(String userId) {
            log.error("Get user authorities {} error", userId, cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.IAM_SERVICE_UNAVAILABLE_ERROR));
        }

        @Override
        public Response<UserAuthority> getClientAuthority() {
            log.error("Get user authorities {} err", cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.IAM_SERVICE_UNAVAILABLE_ERROR));
        }

        @Override
        public Response<List<UserDTO>> findByUserIds(FindByIdsRequest request) {
            log.error("Find all user by ids {} error", request, cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.IAM_SERVICE_UNAVAILABLE_ERROR));
        }

        @Override
        public PagingResponse<UserDTO> autocomplete(PagingRequest request) {
            log.error("Search all user {} error", request, cause);
            if (cause instanceof ForwardInnerAlertException) {
                return PagingResponse.failPaging((RuntimeException) cause);
            }
            return PagingResponse.failPaging(new ResponseException(ServiceUnavailableError.IAM_SERVICE_UNAVAILABLE_ERROR));
        }

        @Override
        public Response<UserDTO> findUserById(String id) {
            log.error("Find User by id {} error", id, cause);
            if(cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.IAM_SERVICE_UNAVAILABLE_ERROR));
        }

        @Override
        public Response<List<DepartmentDTO>> getAllDepartment() {
            log.error("Get all department {0} error", cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.IAM_SERVICE_UNAVAILABLE_ERROR));
        }

        @Override
        public Response<List<DepartmentDTO>> getDepartmentByIds(FindByIdsRequest request) {
            log.error("Find department by ids {} error", request, cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.IAM_SERVICE_UNAVAILABLE_ERROR));
        }

        @Override
        public Response<List<UserDTO>> findByDepartmentIds(FindByIdsRequest request) {
            log.error("Find all user by department ids {} error", request, cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.IAM_SERVICE_UNAVAILABLE_ERROR));
        }

        @Override
        public Response<ClientToken> getTokenClient(ClientLoginRequest request) {
            log.error("Client login {} error", request.getClientId(), cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.IAM_SERVICE_UNAVAILABLE_ERROR));
        }
    }
}
