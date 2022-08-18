package com.evotek.iam.application.config;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.ldapcore.config.LDAPProperties;
import com.evotek.common.ldapcore.service.LDAPService;
import com.evotek.iam.application.service.AuthFailCacheService;
import com.evotek.iam.infrastructure.persistence.entity.UserEntity;
import com.evotek.iam.infrastructure.persistence.repository.UserEntityRepository;
import com.evotek.iam.infrastructure.support.enums.AuthenticationType;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Optional;


@Component
public class CustomAuthenticationProvider implements AuthenticationProvider {
    private final Logger log = LoggerFactory.getLogger(CustomAuthenticationProvider.class);

    private final LDAPProperties ldapProperties;
    private final UserEntityRepository userEntityRepository;
    private final PasswordEncoder passwordEncoder;
    private final AuthFailCacheService authFailCacheService;
    private final LDAPService ldapService;

    public CustomAuthenticationProvider(LDAPProperties ldapProperties, UserEntityRepository userEntityRepository,
                                        PasswordEncoder passwordEncoder,
                                        AuthFailCacheService authFailCacheService,
                                        LDAPService ldapService) {
        this.ldapProperties = ldapProperties;
        this.userEntityRepository = userEntityRepository;
        this.passwordEncoder = passwordEncoder;
        this.authFailCacheService = authFailCacheService;
        this.ldapService = ldapService;
    }

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        log.debug("Authenticating {}", authentication);
        String username = authentication.getName();
        String credentials = (String) authentication.getCredentials();

        Optional<UserEntity> optionalUserEntity = userEntityRepository.findByUsername(username);
        UsernamePasswordAuthenticationToken authenticationToken = (UsernamePasswordAuthenticationToken) authentication;

        if (authenticationToken.isAuthenticated()) {
            if (optionalUserEntity.isEmpty()) {
                // login fail -> increase number of block
                BadRequestError error = authFailCacheService.checkLoginFail(username);
                onError(error);
            }
            UserEntity userEntity = optionalUserEntity.get();
            if (AuthenticationType.LDAP.equals(userEntity.getAuthenticationType())) {
                Boolean hasAttributes = Boolean.TRUE.equals(ldapProperties.getFilteredGroup().getEnabled()) ? ldapService.authenticateUserLDAPInGroup(username, credentials) :
                        ldapService.authenticateUserLDAP(username, credentials);
                if (Boolean.FALSE.equals(hasAttributes)) {
                    // login fail -> increase number of block
                    BadRequestError error = authFailCacheService.checkLoginFail(username);
                    onError(error);
                }
            } else {
                String passwordStore = userEntity.getPassword();
                boolean matches = passwordEncoder.matches(credentials, passwordStore);

                //check UserName Password:
                if (!matches) {
                    // login fail -> increase number of block
                    BadRequestError error = authFailCacheService.checkLoginFail(username);
                    onError(error);
                }
            }
        }
        return new UsernamePasswordAuthenticationToken(username, authentication.getCredentials(), new ArrayList<>());
    }

    private void onError(BadRequestError error) {
        if (error == null) {
            throw new BadCredentialsException("Bad credential!");
        } else {
            throw new ResponseException(error.getMessage(), error);
        }
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return authentication.equals(UsernamePasswordAuthenticationToken.class);
    }

}
