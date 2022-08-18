package com.evotek.iam.application.service.impl;

import com.evotek.iam.application.service.AuthFailCacheService;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.Optional;

@Service
@Slf4j
public class AuthFailCacheServiceImpl implements AuthFailCacheService {
    private final CacheManager cacheManager;

    @Autowired
    public AuthFailCacheServiceImpl(CacheManager cacheManager) {
        this.cacheManager = cacheManager;
    }

    private boolean isExisted(String cacheName, String username) {
        try {
            return Optional.ofNullable(cacheManager.getCache(cacheName))
                    .map(cache -> cache.get(username))
                    .filter(valueWrapper -> Objects.nonNull(valueWrapper.get()))
                    .isPresent();
        } catch (Exception ex) {
            log.error("Cache manager check existed error", ex);
            return false;
        }
    }

    @Override
    public BadRequestError checkLoginFail(String username) {
        try {
            if (!isExisted(LOGIN_FAIL_COUNT_CACHE, username)) {
                Optional.ofNullable(cacheManager.getCache(LOGIN_FAIL_COUNT_CACHE))
                        .ifPresent(cache -> cache.put(username, 1L));
            } else {
                Integer loginFailCount = 0;
                Optional<Cache.ValueWrapper> optionalValueWrapper = Optional.ofNullable(cacheManager.getCache(LOGIN_FAIL_COUNT_CACHE))
                        .map(cache -> cache.get(username))
                        .filter(valueWrapper -> Objects.nonNull(valueWrapper.get()));
                if (optionalValueWrapper.isPresent()) {
                    loginFailCount = (Integer) optionalValueWrapper.get().get();
                }


                if (loginFailCount == null) {
                    Optional.ofNullable(cacheManager.getCache(LOGIN_FAIL_COUNT_CACHE))
                            .ifPresent(cache -> cache.put(username, 1L));
                    return null;
                }

                if (loginFailCount > REACH_MAX_LOGIN_FAIL_COUNT) {
                    Optional.ofNullable(cacheManager.getCache(BLOCKED_USER_LOGIN_CACHE))
                            .ifPresent(cache -> cache.put(username, 1L));
                } else {
                    final Integer newLoginFailCount = loginFailCount + 1;
                    Optional.ofNullable(cacheManager.getCache(LOGIN_FAIL_COUNT_CACHE))
                            .ifPresent(cache -> cache.put(username, newLoginFailCount));

                    if (Objects.equals(newLoginFailCount, REACH_MAX_LOGIN_FAIL_COUNT)) {
                        Optional.ofNullable(cacheManager.getCache(BLOCKED_USER_LOGIN_CACHE))
                                .ifPresent(cache -> cache.put(username, 1L));
                        return BadRequestError.LOGIN_FAIL_BLOCK_ACCOUNT;
                    }
                    if (newLoginFailCount > REACH_WARNING_BEFORE_BLOCK_LOGIN_FAIL_COUNT) {
                        return BadRequestError.LOGIN_FAIL_WARNING_BEFORE_BLOCK;
                    }
                }
            }
        } catch (Exception ex) {
            log.error("Cache manager check login error", ex);
        }
        return null;
    }

    @Override
    public boolean isBlockedUser(String username) {
        return isExisted(BLOCKED_USER_LOGIN_CACHE, username);
    }

    @Override
    public void resetLoginFail(String username) {
        try {
            if (isExisted(LOGIN_FAIL_COUNT_CACHE, username) || isExisted(BLOCKED_USER_LOGIN_CACHE, username)) {
                Optional.ofNullable(cacheManager.getCache(LOGIN_FAIL_COUNT_CACHE))
                        .ifPresent(cache -> cache.evict(username));
                Optional.ofNullable(cacheManager.getCache(BLOCKED_USER_LOGIN_CACHE))
                        .ifPresent(cache -> cache.evict(username));
            }
        } catch (Exception ex) {
            log.error("Cache manager reset login error", ex);
        }
    }
}
