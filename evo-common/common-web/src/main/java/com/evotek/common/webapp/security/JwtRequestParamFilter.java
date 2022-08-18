package com.evotek.common.webapp.security;

import org.springframework.web.filter.OncePerRequestFilter;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Objects;

public class JwtRequestParamFilter extends OncePerRequestFilter {

    private static final String AUTHORIZATION_HEADER = "Authorization";
    private static final String PARAM_KEY_TOKEN = "token";

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {

        HttpServletRequestWrapper wrapper = new HttpServletRequestWrapper(request) {
            @Override
            public String getHeader(String name) {
                // trả về header Authorization nếu có
                if (AUTHORIZATION_HEADER.equals(name)) {
                    if (Objects.nonNull(request.getHeader(AUTHORIZATION_HEADER))) {
                        return request.getHeader(AUTHORIZATION_HEADER);
                    }

                    String header = super.getHeader(name);
                    String tokenInParam = super.getParameter(PARAM_KEY_TOKEN);
                    if (header == null && tokenInParam != null) {
                        header = String.format("bearer %s", tokenInParam.trim());
                    }
                    return header;
                }
                return super.getHeader(name);
            }
        };

        filterChain.doFilter(wrapper, response);
    }
}
