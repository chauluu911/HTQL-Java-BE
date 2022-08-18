package com.evotek.iam.presentation.web.rest.impl;

import com.evotek.common.UserAuthority;
import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.webapp.security.AuthorityService;
import com.evotek.iam.application.config.TemplateProperties;
import com.evotek.iam.application.dto.request.*;
import com.evotek.iam.application.service.UserService;
import com.evotek.iam.domain.User;
import com.evotek.iam.presentation.web.rest.UserResource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

@Slf4j
@RestController
public class UserResourceImpl implements UserResource {

    private final UserService userService;

    private final AuthorityService authorityService;

    private final TemplateProperties templateProperties;

    public UserResourceImpl(UserService userService, AuthorityService authorityService, TemplateProperties templateProperties) {
        this.userService = userService;
        this.authorityService = authorityService;
        this.templateProperties = templateProperties;
    }

    @Override
    public Response<User> create(@Valid UserInternalCreateRequest request) {
        User user = this.userService.create(request);
        return Response.of(user);
    }

    @Override
    public Response<User> update(String id, @Valid UserUpdateRequest request) {
        User user = this.userService.update(id, request);
        return Response.of(user);
    }

    @Override
    public Response<User> getUserById(String id) {
        User user = this.userService.getUserById(id);
        return Response.of(user);
    }

    @Override
    public Response<Void> delete(String id) {
        this.userService.delete(id);
        return Response.ok();
    }

    @Override
    public PagingResponse<User> search(UserSearchRequest request) {
        return userService.search(request);
    }

    @Override
    public Response<Void> deleteByIds(UserDeleteRequest request) {
        this.userService.deleteByIds(request.getIds());
        return Response.ok();
    }

    @Override
    public Response<Boolean> unLock(UserUnLockRequest request) {
        userService.unLock(request);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public Response<List<User>> findByIds(FindByIdsRequest request) {
        List<User> users = userService.findByIds(request.getIds());
        return Response.of(users);
    }

    @Override
    public Response<Boolean> active(String userId) {
        userService.active(userId);
        return Response.of(Boolean.TRUE);
    }

    @Override
    public Response<Boolean> inactive(String userId) {
        userService.inactive(userId);
        return Response.of(Boolean.TRUE);
    }


    @Override
    public Response<UserAuthority> getAuthoritiesByUserId(String userId) {
        UserAuthority userAuthority = authorityService.getUserAuthority(userId);
        return Response.of(userAuthority);
    }

    public Response<User> changePassword(String userId, UserChangePasswordRequest request) {
        return Response.of(this.userService.changePassword(userId, request));
    }

    @Override
    public PagingResponse<User> autocomplete(UserSearchRequest request) {
        return PagingResponse.of(userService.autoComplete(request));
    }

    @Override
    public Response<List<User>> getUserByDepartmentIds(FindByIdsRequest request) {
        return Response.of(userService.findByDepartmentIds(request.getIds()));
    }


    @Override
    public Response<Void> syncUserLdap() {
        userService.syncUserLdap();
        return Response.ok();
    }

    @Override
    public void downloadTemplateExcelImportUser(HttpServletResponse response) {
        this.userService.downloadExcelTemplateFromResource(response);
    }

    @Override
    public void uploadDocument(MultipartFile file, HttpServletResponse response) {
        InputStream inputStream = null;
        try {
            inputStream = file.getInputStream();
            userService.importUser(file, response);
        } catch (IOException e) {
        }
        return;
    }

}
