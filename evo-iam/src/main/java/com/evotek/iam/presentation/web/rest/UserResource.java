package com.evotek.iam.presentation.web.rest;

import com.evotek.common.UserAuthority;
import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.iam.application.dto.request.*;
import com.evotek.iam.domain.User;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.util.List;

@Api(tags = "User Resource")
@RequestMapping("/api")
@Validated
public interface UserResource {

    @ApiOperation(value = "Create user")
    @PostMapping("/users")
    @PreAuthorize("hasPermission(null, 'user:create')")
    Response<User> create(@RequestBody @Valid UserInternalCreateRequest request);

    @ApiOperation(value = "Update user")
    @PostMapping("/users/{id}/update")
    @PreAuthorize("hasPermission(null, 'user:update')")
    Response<User> update(@PathVariable String id, @RequestBody @Valid UserUpdateRequest request);

    @ApiOperation(value = "Get user by Id")
    @GetMapping("/users/{id}")
    @PreAuthorize("hasPermission(null, 'user:view')")
    Response<User> getUserById(@PathVariable String id);

    @ApiOperation(value = "Delete user")
    @PostMapping("/users/{id}/delete")
    @PreAuthorize("hasPermission(null, 'user:delete')")
    Response<Void> delete(@PathVariable String id);

    @ApiOperation(value = "Search user")
    @GetMapping("/users/search")
    @PreAuthorize("hasPermission(null, 'user:view')")
    PagingResponse<User> search(@ValidatePaging(allowedSorts = {"lastModifiedAt", "createdAt", "employeeCode",
            "fullName", "username", "email", "status", "accountType"})
                                        UserSearchRequest request);

    @ApiOperation(value = "Delete users")
    @PostMapping("/users/delete-by-ids")
    @PreAuthorize("hasPermission(null, 'user:delete')")
    Response<Void> deleteByIds(@RequestBody @Valid UserDeleteRequest request);

    @ApiOperation(value = "Unlock account by usernames")
    @PostMapping("/users/unlock-by-usernames")
    @PreAuthorize("hasPermission(null, 'user:update')")
    Response<Boolean> unLock(@RequestBody UserUnLockRequest request);

    @ApiOperation(value = "Find user by ids")
    @PostMapping("/users/find-by-ids")
    @PreAuthorize("hasPermission(null, 'user:view')")
    Response<List<User>> findByIds(@RequestBody FindByIdsRequest request);

    @ApiOperation(value = "Active User")
    @PostMapping("/users/{userId}/active")
    @PreAuthorize("hasPermission(null, 'user:update')")
    Response<Boolean> active(@PathVariable String userId);

    @ApiOperation(value = "Inactive User")
    @PostMapping("/users/{userId}/inactive")
    @PreAuthorize("hasPermission(null, 'user:update')")
    Response<Boolean> inactive(@PathVariable String userId);

    @ApiOperation(value = "Get my authorities")
    @GetMapping("/users/{userId}/authorities")
    Response<UserAuthority> getAuthoritiesByUserId(@PathVariable String userId);

    @ApiOperation(value = "Change password internal user")
    @PostMapping("/users/{userId}/change-password")
    @PreAuthorize("hasPermission(null, 'user:update')")
    Response<User> changePassword(@PathVariable String userId, @RequestBody @Valid UserChangePasswordRequest request);

    @ApiOperation(value = "Search User auto complete")
    @GetMapping("/users/auto-complete")
    @PreAuthorize("hasPermission(null, 'user:view')")
    PagingResponse<User> autocomplete(@ValidatePaging(allowedSorts = {"lastModifiedAt", "createdAt", "employeeCode", "fullName", "username", "email"})
                                              UserSearchRequest request);

    @ApiOperation(value = "Get User by department ids")
    @PostMapping("/users/find-by-department-ids")
    @PreAuthorize("hasPermission(null, 'user:view')")
    Response<List<User>> getUserByDepartmentIds(@RequestBody FindByIdsRequest request);

    @ApiOperation(value = "Sync user ldap")
    @GetMapping("/users/sync-user-ldap")
    @PreAuthorize("hasPermission(null, 'user:update')")
    Response<Void> syncUserLdap();

    @ApiOperation(value = "Import users")
    @PostMapping("/users/import-excel")
    @PreAuthorize("hasPermission(null, 'user:create')")
    void uploadDocument(@RequestParam("file") MultipartFile file,
                        HttpServletResponse response);

    @ApiOperation(value = "download single file")
    @GetMapping("/users/download/template-excel")
    @PreAuthorize("hasPermission(null, 'user:view')")
    void downloadTemplateExcelImportUser(HttpServletResponse response);

}
