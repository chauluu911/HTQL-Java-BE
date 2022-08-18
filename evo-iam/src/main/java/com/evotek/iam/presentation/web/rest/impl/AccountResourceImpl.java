package com.evotek.iam.presentation.web.rest.impl;

import com.evotek.common.UserAuthority;
import com.evotek.common.dto.response.Response;
import com.evotek.common.util.StrUtils;
import com.evotek.common.util.StringPool;
import com.evotek.iam.application.dto.request.*;
import com.evotek.iam.application.dto.response.AuthToken;
import com.evotek.iam.application.service.AccountService;
import com.evotek.iam.domain.User;
import com.evotek.iam.infrastructure.support.util.Const;
import com.evotek.iam.presentation.web.rest.AccountResource;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.view.RedirectView;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

@RestController
public class AccountResourceImpl implements AccountResource {

    private final AccountService accountService;
    @Value("${app.iam.deep-link-reset-password}")
    private String urlResetPassword;

    public AccountResourceImpl(AccountService accountService) {
        this.accountService = accountService;
    }

    @Override
    public Response<User> myProfile() {
        User user = accountService.myProfile();
        return Response.of(user);
    }

    @Override
    public Response<User> updateProfile(@Valid UserUpdateProfileRequest request) {
        User user = accountService.updateProfile(request);
        return Response.of(user);
    }

    @Override
    public Response<User> changePassword(@Valid UserChangePasswordRequest request, HttpServletRequest httpServletRequest) {
        User user = accountService.changePassword(request, httpServletRequest);
        return Response.of(user);
    }

    @Override
    public Response<AuthToken> authenticate(@Valid LoginRequest request) {
        AuthToken authToken = this.accountService.login(request);
        return Response.of(authToken);
    }

    @Override
    public Response<AuthToken> refreshToken(RefreshTokenRequest request) {
        AuthToken authToken = this.accountService.refreshToken(request);
        return Response.of(authToken);
    }

    @Override
    public Response<User> register(@Valid UserRegisterRequest request) {
        User user = accountService.register(request);
        return Response.of(user);
    }

    @Override
    public Response<String> me() {
        String me = this.accountService.currentUser();
        return Response.of(me);
    }

    @Override
    public Response<UserAuthority> myAuthorities() {
        UserAuthority userAuthority = this.accountService.myAuthorities();
        return Response.of(userAuthority);
    }

    @Override
    public Response<Boolean> logout(LogoutRequest request) {
        accountService.logout(request);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public Response<Boolean> initResetPassword(@Valid EmailForgotPasswordRequest request) throws MessagingException {
        accountService.forgotPassword(request);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public Response<Boolean> resetPassword(@Valid ForgotPasswordRequest request) {
        accountService.resetPassword(request);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public RedirectView redirect(String token, HttpServletResponse response) {
        String url;
        if (!StrUtils.isBlank(urlResetPassword)) {
            url = String.format(urlResetPassword, token);
        } else {
            url = Const.DEFAULT_LINK_RESET_PASSWORD;
        }
        response.setHeader(StringPool.LOCATION, url);
        response.setStatus(Const.REDIRECTION_CODE);
        return new RedirectView(url);
    }

}
