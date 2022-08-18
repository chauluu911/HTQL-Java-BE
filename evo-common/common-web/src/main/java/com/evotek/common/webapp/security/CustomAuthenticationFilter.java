package com.evotek.common.webapp.security;

import com.evotek.common.UserAuthentication;
import com.evotek.common.UserAuthority;
import com.evotek.common.enums.AccountType;
import com.evotek.common.enums.UserLevel;
import com.evotek.common.error.AuthenticationError;
import com.evotek.common.exception.ResponseException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.lang.NonNull;
import org.springframework.security.authentication.AbstractAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.filter.OncePerRequestFilter;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.time.Instant;
import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static com.evotek.common.util.Constants.*;

@Component
@Slf4j
public class CustomAuthenticationFilter extends OncePerRequestFilter {

    private final AuthorityService authorityService;
    private final TokenCacheService tokenCacheService;

    public CustomAuthenticationFilter(
            AuthorityService authenticationExtractor, TokenCacheService tokenCacheService) {
        this.authorityService = authenticationExtractor;
        this.tokenCacheService = tokenCacheService;
    }

    @Override
    protected void doFilterInternal(
            @NonNull HttpServletRequest request, @NonNull HttpServletResponse response, @NonNull FilterChain filterChain)
            throws ServletException, IOException {
        log.warn("CustomAuthenticationFilter doFilterInternal");
        SecurityContext securityContext = SecurityContextHolder.getContext();
        JwtAuthenticationToken authentication = (JwtAuthenticationToken) securityContext.getAuthentication();
        Jwt token = authentication.getToken();

        Boolean isRoot = Boolean.FALSE;
        Boolean isClient = Boolean.FALSE;
        String userId = null;
        UserLevel userLevel = null;
        AccountType accountType = null;
        Instant issuedAt = token.getIssuedAt();
        Optional<UserAuthority> optionalUserAuthority = enrichAuthority(token);
        if (issuedAt != null &&
                optionalUserAuthority.isPresent()
                && optionalUserAuthority.get().getLastAuthChangeAt() != null
                && issuedAt.isBefore(optionalUserAuthority.get().getLastAuthChangeAt())) {
            tokenCacheService.invalidToken(token.getTokenValue());
        }

        Set<SimpleGrantedAuthority> grantedPermissions = optionalUserAuthority
                .map(auth -> CollectionUtils.isEmpty(auth.getGrantedPermissions()) ?
                        new HashSet<SimpleGrantedAuthority>() :
                        auth.getGrantedPermissions().stream().map(SimpleGrantedAuthority::new).collect(Collectors.toSet()))
                .orElse(new HashSet<>());

        if (optionalUserAuthority.isPresent()) {
            UserAuthority userAuthority = optionalUserAuthority.get();
            isRoot = userAuthority.getIsRoot();
            userId = userAuthority.getUserId();
            userLevel = userAuthority.getUserLevel();
            accountType = userAuthority.getAccountType();
        } else {
            userId = token.getClaimAsString(USER_ID);
        }

        String authType = token.getClaimAsString(AUTHORITY_TYPE);
        if (CLIENT.equals(authType)) {
            isClient = Boolean.TRUE;
        }

        User principal = new User(authentication.getName(), "", grantedPermissions);
        AbstractAuthenticationToken auth = new UserAuthentication(principal,
                token,
                grantedPermissions,
                isRoot,
                isClient,
                userId,
                userLevel,
                accountType,
                token.getTokenValue());

        SecurityContextHolder.getContext().setAuthentication(auth);
        filterChain.doFilter(request, response);
    }

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {
        SecurityContext securityContext = SecurityContextHolder.getContext();
        Authentication authentication = securityContext.getAuthentication();

        return !(authentication instanceof JwtAuthenticationToken);
    }

    private Optional<UserAuthority> enrichAuthority(Jwt token) {
        String identifier = token.getSubject();
        String authType = token.getClaimAsString(AUTHORITY_TYPE);
        try {
            if (CLIENT.equals(authType)) {
                log.warn("CustomAuthenticationFilter enrichAuthority clientId {}", identifier);
                UserAuthority userAuthority = authorityService.getClientAuthority(identifier);
                if (Objects.nonNull(userAuthority)) {
                    log.warn("CustomAuthenticationFilter enrichAuthority client {} success", identifier);
                    userAuthority.setUserId(identifier);
                    return Optional.of(userAuthority);
                }
                log.warn("CustomAuthenticationFilter enrichAuthority client {} fail", identifier);
            } else {
                String userId = token.getClaimAsString(USER_ID);
                if (!StringUtils.hasLength(userId)) {
                    log.warn("CustomAuthenticationFilter enrichAuthority username {} fail, userId = null", identifier);
                    return Optional.empty();
                }
                log.warn("CustomAuthenticationFilter enrichAuthority userId {}", userId);
                UserAuthority userAuthority = authorityService.getUserAuthority(userId);
                if (userAuthority != null) {
                    log.warn("CustomAuthenticationFilter enrichAuthority userId {} success", userId);
                    userAuthority.setUserId(userId);
                    return Optional.of(userAuthority);
                }
                log.warn("CustomAuthenticationFilter enrichAuthority userId {} fail", userId);
            }
        } catch (ResponseException e) {
            if (e.getError().equals(AuthenticationError.FORBIDDEN_ACCESS_TOKEN)) {
                throw new ResponseException(AuthenticationError.UNAUTHORISED);
            }
        } catch (HttpClientErrorException.Unauthorized e) {
            throw new ResponseException(AuthenticationError.UNAUTHORISED);
        } catch (Exception e) {
            log.warn("Failed to enrich authorities for client or user {}", identifier, e);
        }
        return Optional.empty();
    }
}
