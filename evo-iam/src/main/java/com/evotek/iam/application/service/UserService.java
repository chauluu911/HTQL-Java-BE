package com.evotek.iam.application.service;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.iam.application.dto.request.*;
import com.evotek.iam.domain.User;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

public interface UserService {

    User create(UserInternalCreateRequest request);

    User update(String userId, UserUpdateRequest request);

    void delete(String id);

    PagingResponse<User> search(UserSearchRequest request);

    void deleteByIds(List<String> ids);

    void unLock(UserUnLockRequest request);

    List<User> findByIds(List<String> ids);

    void active(String userId);

    void inactive(String userId);

    User getUserById(String userId);

    User changePassword(String userId, UserChangePasswordRequest request);

    PageDTO<User> autoComplete(UserSearchRequest request);

    void importUser(MultipartFile file, HttpServletResponse httpResponse);

    void syncUserLdap();

    List<User> findByDepartmentIds(List<String> ids);

    void downloadExcelTemplateFromResource(HttpServletResponse response);

}
