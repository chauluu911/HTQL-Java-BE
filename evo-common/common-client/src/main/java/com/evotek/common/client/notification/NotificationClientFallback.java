package com.evotek.common.client.notification;

import com.evotek.common.dto.request.LogoutRevokeRequest;
import com.evotek.common.dto.request.notification.IssueEventRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.common.error.ServiceUnavailableError;
import com.evotek.common.exception.ForwardInnerAlertException;
import com.evotek.common.exception.ResponseException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cloud.openfeign.FallbackFactory;
import org.springframework.stereotype.Component;

@Component
public class NotificationClientFallback implements FallbackFactory<NotificationClient> {

    @Override
    public NotificationClient create(Throwable cause) {
        return new FallbackWithFactory(cause);
    }

    @Slf4j
    static class FallbackWithFactory implements NotificationClient {

        private final Throwable cause;

        FallbackWithFactory(Throwable cause) {
            this.cause = cause;
        }

        @Override
        public Response<Boolean> revokeDevice(LogoutRevokeRequest request) {
            log.error("Revoke device {} error", request, cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.NOTIFICATION_SERVICE_UNAVAILABLE_ERROR));
        }

        @Override
        public Response<Void> issueEvent(IssueEventRequest request) {
            log.error("Issue event {} error", request, cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.NOTIFICATION_SERVICE_UNAVAILABLE_ERROR));
        }
    }
}
