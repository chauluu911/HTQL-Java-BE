package com.evotek.iam.application.service;

import com.evotek.common.UserAuthority;
import com.evotek.iam.application.dto.request.*;
import com.evotek.iam.application.dto.response.AuthToken;
import com.evotek.iam.domain.User;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletRequest;

public interface AccountService {

    AuthToken login(LoginRequest request);

    AuthToken refreshToken(RefreshTokenRequest request);

    User register(UserRegisterRequest request);

    User myProfile();

    User updateProfile(UserUpdateProfileRequest request);

    User changePassword(UserChangePasswordRequest request, HttpServletRequest httpServletRequest);

    String currentUser();

    UserAuthority myAuthorities();

    void logout(LogoutRequest request);

    void forgotPassword(EmailForgotPasswordRequest request) throws MessagingException;

    void resetPassword(ForgotPasswordRequest request);
}
