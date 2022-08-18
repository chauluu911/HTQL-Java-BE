package com.evotek.common.client.system;

import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.system.ConfigurationDTO;
import com.evotek.common.enums.ConfigurationType;
import com.evotek.common.error.ServiceUnavailableError;
import com.evotek.common.exception.ForwardInnerAlertException;
import com.evotek.common.exception.ResponseException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cloud.openfeign.FallbackFactory;
import org.springframework.stereotype.Component;

@Component
public class SystemClientFallback implements FallbackFactory<SystemClient> {
    @Override
    public SystemClient create(Throwable cause) {
        return new FallbackWithFactory(cause);
    }

    @Slf4j
    static class FallbackWithFactory implements SystemClient {

        private final Throwable cause;

        FallbackWithFactory(Throwable cause) {
            this.cause = cause;
        }

        @Override
        public Response<ConfigurationDTO> findByCode(ConfigurationType code) {
            log.error("Get Configuration by code {} error", code, cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.SYSTEM_SERVICE_UNAVAILABLE_ERROR));
        }
    }
}
