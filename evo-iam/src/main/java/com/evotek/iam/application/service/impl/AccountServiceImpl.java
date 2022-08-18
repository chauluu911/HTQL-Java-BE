package com.evotek.iam.application.service.impl;

import com.evotek.common.UserAuthority;
import com.evotek.common.client.notification.NotificationClient;
import com.evotek.common.client.storage.StorageClient;
import com.evotek.common.dto.request.LogoutRevokeRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.storage.FileDTO;
import com.evotek.common.error.AuthenticationError;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.ldapcore.service.LDAPService;
import com.evotek.common.webapp.security.AuthorityService;
import com.evotek.common.webapp.support.SecurityUtils;
import com.evotek.iam.application.config.AuthenticationProperties;
import com.evotek.iam.application.config.TokenProvider;
import com.evotek.iam.application.dto.request.*;
import com.evotek.iam.application.dto.response.AuthToken;
import com.evotek.iam.application.mapper.AutoMapper;
import com.evotek.iam.application.service.AccountService;
import com.evotek.iam.application.service.AuthFailCacheService;
import com.evotek.iam.application.service.SendEmailService;
import com.evotek.iam.domain.User;
import com.evotek.iam.domain.command.UserRegisterCmd;
import com.evotek.iam.domain.command.UserUpdateProfileCmd;
import com.evotek.iam.domain.respository.UserDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.UserEntity;
import com.evotek.iam.infrastructure.persistence.mapper.UserEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.UserEntityRepository;
import com.evotek.iam.infrastructure.support.enums.AuthenticationType;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import com.evotek.iam.infrastructure.support.exception.NotFoundError;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletRequest;
import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.Objects;
import java.util.Optional;

@Service
@Slf4j
@RequiredArgsConstructor
public class AccountServiceImpl implements AccountService {

    private static final String TITLE_KEY = "EMAIL_RESET_PASSWORD";
    private static final String TEMPLATE_NAME = "mail/passwordResetEmail";

    private final UserDomainRepository userDomainRepository;
    private final UserEntityRepository userEntityRepository;
    private final UserEntityMapper userEntityMapper;
    private final PasswordEncoder passwordEncoder;
    private final AutoMapper autoMapper;
    private final TokenProvider tokenProvider;
    private final AuthenticationProperties authenticationProperties;
    private final AuthenticationManager authenticationManager;
    private final AuthorityService authorityService;
    private final NotificationClient notificationClient;
    private final AuthFailCacheService authFailCacheService;
    private final SendEmailService sendEmailService;
    private final StorageClient storageClient;
    private final LDAPService ldapService;

    @Override
    public AuthToken login(LoginRequest request) {
        log.warn("User {} start login", request.getUsername());

        // check account was locked
        if (authFailCacheService.isBlockedUser(request.getUsername())) {
            log.warn("User {} is blocked", request.getUsername());
            throw new ResponseException(BadRequestError.LOGIN_FAIL_BLOCK_ACCOUNT);
        }

        // check user
        Optional<UserEntity> optionalUserEntity = this.userEntityRepository.findByUsername(request.getUsername());
        if (optionalUserEntity.isEmpty()) {
            BadRequestError error = authFailCacheService.checkLoginFail(request.getUsername());
            log.warn("User login not found: {}", request.getUsername());
            if (error == null) {
                throw new BadCredentialsException("Bad credential!");
            } else {
                throw new ResponseException(error.getMessage(), error);
            }
        }
        UserEntity userEntity = optionalUserEntity.get();
        if (!UserStatus.ACTIVE.equals(userEntity.getStatus())) {
            log.warn("User login not activated: {}", request.getUsername());
            authFailCacheService.checkLoginFail(userEntity.getUsername());
            throw new ResponseException(BadRequestError.LOGIN_FAIL_BLOCK_ACCOUNT);
        }

        Authentication authentication = new UsernamePasswordAuthenticationToken(request.getUsername().toLowerCase(),
                request.getPassword(), new ArrayList<>());
        authentication = authenticationManager.authenticate(authentication);

        String accessToken = this.tokenProvider.createToken(authentication, userEntity.getId());
        long expiresIn = this.authenticationProperties.getAccessTokenExpiresIn().toSeconds();
        String refreshToken = null;
        if (request.isRememberMe()) {
            refreshToken = this.tokenProvider.createRefreshToken(userEntity.getId());
        }
        log.warn("User {} login success", request.getUsername());

        authFailCacheService.resetLoginFail(request.getUsername());
        return AuthToken.builder()
                .accessToken(accessToken)
                .refreshToken(refreshToken)
                .tokenType(AuthToken.TOKEN_TYPE_BEARER)
                .expiresIn(expiresIn)
                .build();
    }

    @Override
    public AuthToken refreshToken(RefreshTokenRequest request) {
        if (!this.tokenProvider.validateRefreshToken(request.getRefreshToken())) {
            throw new ResponseException(AuthenticationError.INVALID_REFRESH_TOKEN);
        }

        String userId = this.tokenProvider.getSubject(request.getRefreshToken());
        UserEntity userEntity = this.userEntityRepository.findById(userId).orElseThrow(() ->
                new ResponseException(NotFoundError.USER_NOT_FOUND));
        if (!UserStatus.ACTIVE.equals(userEntity.getStatus())) {
            authFailCacheService.checkLoginFail(userEntity.getUsername());
            throw new ResponseException(BadRequestError.LOGIN_FAIL_BLOCK_ACCOUNT);
        }

        Authentication authentication = new UsernamePasswordAuthenticationToken(userEntity.getUsername(),
                "",
                new ArrayList<>());
        String accessToken = this.tokenProvider.createToken(authentication, userEntity.getId());
        String refreshToken = this.tokenProvider.createRefreshToken(userEntity.getId());

        long expiresIn = this.authenticationProperties.getAccessTokenExpiresIn().toSeconds();

        return AuthToken.builder()
                .accessToken(accessToken)
                .refreshToken(refreshToken)
                .tokenType(AuthToken.TOKEN_TYPE_BEARER)
                .expiresIn(expiresIn)
                .build();
    }

    @Override
    public User register(UserRegisterRequest request) {
        Optional<UserEntity> optionalUserEntity = this.userEntityRepository.findByUsername(request.getUsername());
        if (optionalUserEntity.isPresent()) {
            throw new ResponseException(BadRequestError.USER_USERNAME_EXITED);
        }

        Optional<UserEntity> userEntityByEmail = userEntityRepository.findByEmail(request.getEmail());
        if (userEntityByEmail.isPresent()) {
            throw new ResponseException(BadRequestError.EMAIL_NOT_EXISTED_IN_SYSTEM);
        }

        Optional<UserEntity> userEntityByPhone = userEntityRepository.findByPhoneNumber(request.getPhoneNumber());
        if (userEntityByPhone.isPresent()) {
            throw new ResponseException(BadRequestError.USER_PHONE_NUMBER_EXITED);
        }

        UserRegisterCmd cmd = this.autoMapper.from(request);
        String encodedPassword = this.passwordEncoder.encode(cmd.getPassword());
        cmd.setPassword(encodedPassword);
        User user = new User(cmd);
        this.userDomainRepository.save(user);
        return user;
    }

    @Override
    public User myProfile() {
        return currentAccount();
    }

    @Override
    public User updateProfile(UserUpdateProfileRequest request) {
        User user = currentAccount();

        if (!Objects.equals(request.getPhoneNumber(), user.getPhoneNumber())) {
            Optional<UserEntity> userEntityByPhone = userEntityRepository
                    .findByPhoneNumber(request.getPhoneNumber());
            if (userEntityByPhone.isPresent()) {
                throw new ResponseException(BadRequestError.USER_PHONE_NUMBER_EXITED);
            }
        }

        if (!user.getEmail().equals(request.getEmail())) {
            Optional<UserEntity> userEntityByEmail = userEntityRepository.findByEmail(request.getEmail());
            if (userEntityByEmail.isPresent()) {
                throw new ResponseException(BadRequestError.USER_EMAIL_EXITED);
            }
        }
        UserUpdateProfileCmd cmd = this.autoMapper.from(request);
        user.updateProfile(cmd);
        this.userDomainRepository.save(user);
        return user;
    }

    @Transactional
    @Override
    public User changePassword(UserChangePasswordRequest request, HttpServletRequest httpServletRequest) {
        User user = currentAccount();
        if (!this.passwordEncoder.matches(request.getOldPassword(), user.getPassword())) {
            throw new ResponseException(BadRequestError.WRONG_PASSWORD);
        }

//         invalid token and refresh token
        tokenProvider.invalidJwt(tokenProvider.resolveToken(httpServletRequest), request.getRefreshToken());

        String newEncodedPassword = this.passwordEncoder.encode(request.getNewPassword());
        if (Objects.equals(user.getAuthenticationType(), AuthenticationType.LDAP)) {
            boolean result = this.ldapService.resetPassUserLdap(user.getFullName(), this.autoMapper.from(request));
            if (!result) {
                throw new ResponseException(BadRequestError.ACCOUNT_LDAP_CAN_NOT_CHANGE_PASSWORD);
            }
        }
        user.changePassword(newEncodedPassword);
        this.userDomainRepository.save(user);
        return user;
    }

    public String currentUserId() {
        Optional<String> currentUserLoginId = SecurityUtils.getCurrentUserLoginId();
        if (currentUserLoginId.isEmpty()) {
            throw new ResponseException(AuthenticationError.UNAUTHORISED);
        }
        return currentUserLoginId.get();
    }

    @Override
    public UserAuthority myAuthorities() {
        String me = currentUserId();
        return this.authorityService.getUserAuthority(me);
    }

    @Override
    public void logout(LogoutRequest logoutRequest) {
        // Neu mobile logout thi revoke token device
        if (Objects.nonNull(logoutRequest.getDeviceToken())) {
            String userId = currentUserId();
            LogoutRevokeRequest logoutRevokeRequest = LogoutRevokeRequest.builder()
                    .deviceId(logoutRequest.getDeviceId())
                    .deviceToken(logoutRequest.getDeviceToken())
                    .userId(userId)
                    .build();
            notificationClient.revokeDevice(logoutRevokeRequest);
        }

        // revoke device -> invalid token and refresh token
        Optional<String> optionalJwt = SecurityUtils.getCurrentUserJWT();
        if (optionalJwt.isPresent()) {
            String accessToken = optionalJwt.get();
            tokenProvider.invalidJwt(accessToken, logoutRequest.getRefreshToken());
        }
    }

    @Override
    public void forgotPassword(EmailForgotPasswordRequest request) throws MessagingException {
        Optional<UserEntity> userEntityByEmail = userEntityRepository.findByEmail(request.getEmail());
        if (userEntityByEmail.isEmpty()) {
            log.warn("Password reset requested for non existing mail '{}'", request.getEmail());
            throw new ResponseException(BadRequestError.EMAIL_NOT_EXISTED_IN_SYSTEM);
        }

        if (userEntityByEmail.get().getAuthenticationType().equals(AuthenticationType.LDAP)) {
            throw new ResponseException(BadRequestError.ACCOUNT_EMPLOYEE_CAN_NOT_CHANGE_PASSWORD);
        }

        User user = userEntityMapper.toDomain(userEntityByEmail.get());
        String token = tokenProvider.createTokenSendEmail(user.getId(), request.getEmail());
        sendEmailService.send(user, TEMPLATE_NAME, TITLE_KEY, token);
    }

    @Override
    public void resetPassword(ForgotPasswordRequest request) {
        String userId = tokenProvider.validateEmailToken(request.getToken());
        User user = this.userDomainRepository.getById(userId);
        if (!Objects.equals(request.getPassword(), request.getRepeatPassword())) {
            throw new ResponseException(BadRequestError.REPEAT_PASSWORD_DOES_NOT_MATCH);
        }
        String encodedPassword = this.passwordEncoder.encode(request.getPassword());
        user.changePassword(encodedPassword);
        userDomainRepository.save(user);
    }

    @Override
    public String currentUser() {
        Optional<String> currentUser = SecurityUtils.getCurrentUser();
        if (currentUser.isEmpty()) {
            throw new ResponseException(AuthenticationError.UNAUTHORISED);
        }
        return currentUser.get();
    }

    private User currentAccount() {
        String userId = currentUserId();
        User user = this.userDomainRepository.getById(userId);

        if (Objects.nonNull(user.getAvatarFileId())) {
            Response<FileDTO> responseFile = storageClient.findById(user.getAvatarFileId());
            if (responseFile.isSuccess() && Objects.nonNull(responseFile.getData())) {
                user.enrichViewUrlFile(responseFile.getData().getViewUrl());
            }
        }
        return user;
    }
}
