package com.evotek.common.webapp.security;

import com.evotek.common.UserAuthentication;
import com.evotek.common.error.AuthorizationError;
import com.evotek.common.exception.ResponseException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.PermissionEvaluator;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.text.MessageFormat;
import java.util.regex.Pattern;

@Slf4j
@Component
public class RegexPermissionEvaluator implements PermissionEvaluator {

    @Override
    public boolean hasPermission(Authentication authentication, Object targetDomainObject, Object permission) {
        String requiredPermission = permission.toString();

        log.warn("RegexPermissionEvaluator hasPermission");

        if (!(authentication instanceof UserAuthentication)) {
            throw new ResponseException(
                    MessageFormat.format(
                            AuthorizationError.NOT_SUPPORTED_AUTHENTICATION.getMessage(),
                            authentication.getClass().getName()),
                    AuthorizationError.NOT_SUPPORTED_AUTHENTICATION, authentication.getClass().getName());
        }
        UserAuthentication userAuthentication = (UserAuthentication) authentication;

        if (userAuthentication.isRoot()) {
            return true;
        }

        return userAuthentication.getGrantedPermissions().stream()
                .anyMatch(p -> Pattern.matches(p, requiredPermission));
    }

    @Override
    public boolean hasPermission(
            Authentication authentication, Serializable targetId, String targetType, Object permission) {
        return hasPermission(authentication, null, permission);
    }
}
