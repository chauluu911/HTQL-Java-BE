package com.evotek.common.webapp.config;

import com.evotek.common.UserAuthentication;
import com.evotek.common.queue.message.ActionLogMessage;
import com.evotek.common.util.Constants;
import com.evotek.common.util.StringPool;
import com.evotek.common.webapp.support.CachedHttpServletRequestWrapper;
import com.evotek.common.webapp.support.SecurityUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.core.annotation.Order;
import org.springframework.util.StringUtils;
import org.springframework.web.util.ContentCachingResponseWrapper;

import javax.servlet.*;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Objects;

@WebFilter("/api/**")
@Slf4j
@Order(100)
@ConditionalOnExpression("${spring.rabbitmq.enabled:false}")
public class ActionLogFilter implements Filter {

    private final List<String> blackList = List.of("\\/api\\/certificate\\/.well-known\\/jwks\\.json",
            ".*\\/actuator\\/.*", "\\/api\\/action-logs.*",
            "/swagger-ui.*",
            "/swagger-resources.*",
            "/v2/api-docs.*");
    private final List<String> blackListMimeType = List.of("multipart\\/form-data.*",
            "image\\/.*",
            "application\\/octet-stream.*");

    //    private final RabbitTemplate rabbitTemplate;
    private final String module;

    public ActionLogFilter(@Value(value = "${spring.application.name}") String module) {
        this.module = module;
    }

    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain)
            throws IOException, ServletException {
        Instant start = Instant.now();

        // check request content type
        HttpServletRequest httpServletRequest = ((HttpServletRequest) servletRequest);
        String requestContentType = httpServletRequest.getHeader("Content-Type");
        log.info("requestContentType: {}", requestContentType);
        boolean ignoredRequestBody = Objects.nonNull(requestContentType)
                && this.blackListMimeType.stream().anyMatch(requestContentType::matches);
        ContentCachingResponseWrapper cachedResponse
                = new ContentCachingResponseWrapper((HttpServletResponse) servletResponse);
        cachedResponse.setCharacterEncoding(StringPool.UTF8);

        if (!ignoredRequestBody) {
            httpServletRequest = new CachedHttpServletRequestWrapper((HttpServletRequest) servletRequest);
        }
        try {
            filterChain.doFilter(httpServletRequest, cachedResponse);
        } finally {
            Instant finishRequest = Instant.now();
            Instant startCheck = Instant.now();

            String responseStr = "";
            byte[] responseArray = cachedResponse.getContentAsByteArray();
            cachedResponse.copyBodyToResponse();
            // check response content type
            String responseContentType = cachedResponse.getHeader("Content-Type");
            log.info("responseContentType: {}", responseContentType);
            boolean ignoredResponse = Objects.nonNull(responseContentType)
                    && this.blackListMimeType.stream().anyMatch(responseContentType::matches);
            if (!ignoredResponse) {
                responseStr = new String(responseArray, cachedResponse.getCharacterEncoding());
            }
            log.info("should log: {}", this.shouldFilter(httpServletRequest));
            if (shouldFilter((HttpServletRequest) servletRequest)) {

                String method = ((HttpServletRequest) servletRequest).getMethod();
                long time = Duration.between(start, finishRequest).toMillis();
                ActionLogMessage actionLogRequest = ActionLogMessage.builder()
                        .uri(((HttpServletRequest) servletRequest).getRequestURI())
                        .method(method)
                        .userId(SecurityUtils.getCurrentUserLoginId().orElse(Constants.ANONYMOUS_ACCOUNT))
                        .remoteIp(this.getRemoteIp(httpServletRequest))
                        .userAgent(((HttpServletRequest) servletRequest).getHeader(StringPool.USER_AGENT))
                        .module(module)
                        .duration(time)
                        .responsePayload(responseStr)
                        .httpCode(cachedResponse.getStatus())
                        .hostIp(servletRequest.getLocalAddr())
                        .requestParam(httpServletRequest.getQueryString())
                        .accessedAt(start)
                        .sentAt(Instant.now())
                        .sentBy(module)
                        .build();

                if (httpServletRequest instanceof CachedHttpServletRequestWrapper
                        && (!ignoredRequestBody)
                        && method.equalsIgnoreCase(StringPool.POST)) {
                    String body = ((CachedHttpServletRequestWrapper) httpServletRequest).getBody();
                    actionLogRequest.setRequestPayload(this.replaceRequestBody(body));
                }
//                this.rabbitTemplate.convertAndSend(QueueBindingConfig.ActionLog.EXCHANGE_NAME,
//                        QueueBindingConfig.ActionLog.ROUTING_KEY,
//                        actionLogRequest);
                log.info("Send action log time: {} ms and {} ns",
                        Duration.between(startCheck, Instant.now()).toMillis(),
                        Duration.between(startCheck, Instant.now()).toNanos());
            }
        }
    }

    private boolean shouldFilter(HttpServletRequest request) {
        try {
            UserAuthentication user = SecurityUtils.authentication();
            if (user.isClient()) {
                return false;
            }
        } catch (Exception e) {
            log.debug("Action log, ignore exception", e);
        }
//        check isBlack list
        if (this.blackList.isEmpty()) {
            return true;
        }
        String uri = String.valueOf(request.getRequestURI());
        return this.blackList.stream().noneMatch(uri::matches);
    }

    private String replaceRequestBody(String body) {
        if (!StringUtils.hasLength(body)) {
            return body;
        }
        return body.replaceAll("\"password\":\"(.*?)\"", "\"password\":\"******\"")
                .replaceAll("\"clientSecret\":\"(.*?)\"", "\"clientSecret\":\"******\"");
    }

    private String getRemoteIp(HttpServletRequest request) {
        String ip = request.getHeader("X-Forwarded-For");
        if (StringUtils.hasLength(ip) && !"unKnown".equalsIgnoreCase(ip)) {
            int index = ip.indexOf(",");
            if (index != -1) {
                log.info("get remote ip: {}", ip);
                return ip.substring(0, index);
            } else {
                return ip;
            }
        }
        ip = request.getHeader("X-Real-IP");
        if (StringUtils.hasLength(ip) && !"unKnown".equalsIgnoreCase(ip)) {
            return ip;
        }
        return request.getRemoteAddr();
    }
}
