package com.evotek.iam.application.service.impl;

import com.evotek.common.client.storage.StorageClient;
import com.evotek.common.dto.PageDTO;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.storage.FileDTO;
import com.evotek.common.enums.Gender;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.ldapcore.service.LDAPService;
import com.evotek.common.util.DateUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.common.util.StringPool;
import com.evotek.common.validator.ValidateConstraint;
import com.evotek.iam.application.config.TemplateProperties;
import com.evotek.iam.application.dto.request.*;
import com.evotek.iam.application.dto.response.ImportUserDTO;
import com.evotek.iam.application.mapper.AutoMapper;
import com.evotek.iam.application.mapper.AutoMapperQuery;
import com.evotek.iam.application.service.AuthFailCacheService;
import com.evotek.iam.application.service.SyncService;
import com.evotek.iam.application.service.UserService;
import com.evotek.iam.domain.*;
import com.evotek.iam.domain.command.ImportUserCmd;
import com.evotek.iam.domain.command.UserInternalCreateCmd;
import com.evotek.iam.domain.command.UserUpdateCmd;
import com.evotek.iam.domain.query.UserSearchQuery;
import com.evotek.iam.domain.respository.UserDomainRepository;
import com.evotek.iam.infrastructure.persistence.entity.*;
import com.evotek.iam.infrastructure.persistence.mapper.DepartmentEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.UserGroupMemberEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.JobTitleEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.RoleEntityMapper;
import com.evotek.iam.infrastructure.persistence.mapper.UserEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.*;
import com.evotek.iam.infrastructure.support.enums.AuthenticationType;
import com.evotek.iam.infrastructure.support.enums.RoleStatus;
import com.evotek.iam.infrastructure.support.enums.UserStatus;
import com.evotek.iam.infrastructure.support.exception.BadRequestError;
import com.evotek.iam.infrastructure.support.exception.NotFoundError;
import com.evotek.iam.infrastructure.support.util.ExcelUtils;
import com.evotek.iam.infrastructure.support.util.LabelKey;
import com.evotek.iam.infrastructure.support.util.Labels;
import com.evotek.iam.infrastructure.support.util.StringUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.jxls.common.Context;
import org.jxls.util.JxlsHelper;
import org.springframework.core.io.ClassPathResource;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;


@Service
@Slf4j
@RequiredArgsConstructor
public class UserServiceImpl implements UserService {

    static final int ROW_NUMBER = 0;
    private final UserDomainRepository userDomainRepository;
    private final UserEntityRepository userEntityRepository;
    private final UserEntityMapper userEntityMapper;
    private final PasswordEncoder passwordEncoder;
    private final AutoMapper autoMapper;
    private final AuthFailCacheService authFailCacheService;
    private final UserRoleEntityRepository userRoleEntityRepository;
    private final RoleEntityRepository roleEntityRepository;
    private final RoleEntityMapper roleEntityMapper;
    private final StorageClient storageClient;
    private final AutoMapperQuery autoMapperQuery;
    private final DepartmentEntityRepository departmentEntityRepository;
    private final DepartmentEntityMapper departmentEntityMapper;
    private final SyncService syncService;
    private final LDAPService ldapService;
    private final TemplateProperties templateProperties;
    private final JobTitleEntityRepository jobTitleEntityRepository;
    private final JobTitleEntityMapper jobTitleEntityMapper;
    private final UserGroupMemberEntityRepository userGroupMemberEntityRepository;
    private final UserGroupMemberEntityMapper userGroupMemberEntityMapper;

    @Override
    @Transactional
    public User create(UserInternalCreateRequest request) {
        // valid base user information
        validateBaseInformation(request);
        if (Objects.nonNull(request.getJobTitleId())) {
            Optional<JobTitleEntity> jobTitleEntity = jobTitleEntityRepository.findById(request.getJobTitleId());
            if (jobTitleEntity.isEmpty()) {
                throw new ResponseException(BadRequestError.JOB_TITLE_DOES_NOT_EXISTED);
            }
        } else {
            throw new ResponseException(BadRequestError.JOB_TITLE_ID_REQUIRED);
        }

        UserInternalCreateCmd cmd = this.autoMapper.from(request);

        String encodedPassword = this.passwordEncoder.encode(cmd.getPassword());
        cmd.setPassword(encodedPassword);

        List<RoleEntity> roleEntities = roleEntityRepository.findAllByStatus(RoleStatus.ACTIVE);
        User user = new User(cmd, roleEntityMapper.toDomain(roleEntities));
        this.userDomainRepository.save(user);

        if (Objects.equals(request.getAuthenticationType(), AuthenticationType.LDAP)) {
            ldapService.createUserLdap(this.autoMapper.fromInternal(request));
        }
        return user;
    }

    @Override
    @Transactional
    public User update(String userId, UserUpdateRequest request) {
        User user = this.userDomainRepository.getById(userId);
        // valid user by email
        if (!user.getEmail().equals(request.getEmail())) {
            Optional<UserEntity> userEntityByEmail = userEntityRepository.findByEmail(request.getEmail());
            if (userEntityByEmail.isPresent()) {
                throw new ResponseException(BadRequestError.USER_EMAIL_EXITED);
            }
        }

        // check phone number
        if (!Objects.equals(request.getPhoneNumber(), user.getPhoneNumber())) {
            Optional<UserEntity> userEntityByPhone = userEntityRepository
                    .findByPhoneNumber(request.getPhoneNumber());
            if (userEntityByPhone.isPresent()) {
                throw new ResponseException(BadRequestError.USER_PHONE_NUMBER_EXITED);
            }
        }

        // valid user by employee code
        if (user.getEmployeeCode() != null && !Objects.equals(user.getEmployeeCode(), request.getEmployeeCode())) {
            Optional<UserEntity> userEntityEmployeeCode = userEntityRepository.findByEmployeeCode(request.getEmployeeCode());
            if (userEntityEmployeeCode.isPresent()) {
                throw new ResponseException(BadRequestError.USER_EMPLOYEE_CODE_EXITED);
            }
        }

        if (Objects.nonNull(request.getAvatarFileId())) {
            Response<FileDTO> responseFile = storageClient.findById(request.getAvatarFileId());
            if (!responseFile.isSuccess()) {
                throw new ResponseException(BadRequestError.FILE_NOT_EXISTED);
            }
        }
        if (!user.getJobTitleId().equals(request.getJobTitleId())){
            if (Objects.nonNull(request.getJobTitleId())) {
                Optional<JobTitleEntity> jobTitleEntity = jobTitleEntityRepository.findById(request.getJobTitleId());
                if (jobTitleEntity.isEmpty()) {
                    throw new ResponseException(BadRequestError.JOB_TITLE_DOES_NOT_EXISTED);
                }
            } else {
                throw new ResponseException(BadRequestError.JOB_TITLE_ID_REQUIRED);
            }
        }

        UserUpdateCmd cmd = this.autoMapper.from(request);
        List<RoleEntity> roleEntities = roleEntityRepository.findAllByStatus(RoleStatus.ACTIVE);
        user.update(cmd, roleEntityMapper.toDomain(roleEntities));
        this.userDomainRepository.save(user);
        return user;
    }

    @Override
    @Transactional
    public void delete(String userId) {
        User user = this.userDomainRepository.getById(userId);
        user.deleted();
        this.userDomainRepository.save(user);
    }

    @Override
    public PagingResponse<User> search(UserSearchRequest request) {
        UserSearchQuery query = autoMapperQuery.toQuery(request);
        List<UserGroupMember> userGroupMembers = this.userGroupMemberEntityMapper.toDomain(this.userGroupMemberEntityRepository.findAllMemberByGroupIds(request.getGroupIds()));
        List<String> listUserId = userGroupMembers.stream().map(UserGroupMember::getUserId).distinct().collect(Collectors.toList());
        query.setUserIds(listUserId);
        List<UserEntity> userEntities = userEntityRepository.search(query);
        List<User> users = userEntityMapper.toDomain(userEntities);
        List<String> userIds = users.stream().map(User::getId).distinct().collect(Collectors.toList());
        // enrich roles
        List<RoleEntity> roleEntities = roleEntityRepository.findAllByStatus(RoleStatus.ACTIVE);
        List<Role> roles = roleEntityMapper.toDomain(roleEntities);
        List<UserRoleEntity> userRoleEntities = userRoleEntityRepository.findAllByUserIds(userIds);

        // get list department
        List<String> departmentIds = users.stream().map(User::getDepartmentId).distinct().collect(Collectors.toList());
        List<DepartmentEntity> departmentEntities = departmentEntityRepository.findAllById(departmentIds);
        List<Department> departments = departmentEntityMapper.toDomain(departmentEntities);

        users.forEach(user -> {
            List<String> roleIdsOfUser = userRoleEntities.stream()
                    .filter(userRole -> userRole.getUserId().equals(user.getId()))
                    .map(UserRoleEntity::getRoleId)
                    .distinct().collect(Collectors.toList());
            List<Role> rolesOfUser = roles.stream()
                    .filter(role -> roleIdsOfUser.contains(role.getId())).collect(Collectors.toList());
            user.enrichRoles(rolesOfUser);
            // enrich department
            departments.forEach(department -> {
                if (department.getId().equals(user.getDepartmentId())) {
                    user.enrichDepartment(department);
                }
            });
            if(Objects.nonNull(user.getJobTitleId())){
                Optional<JobTitleEntity> jobTitleEntity = jobTitleEntityRepository.findById(user.getJobTitleId());
                if(jobTitleEntity.isPresent()) {
                    JobTitleEntity j = jobTitleEntity.get();
                    user.enrichJobTitleName(j.getName());
                }
            }

        });
        return new PagingResponse<>(users,
                request.getPageIndex(),
                request.getPageSize(),
                userEntityRepository.countUser(query));
    }

    @Override
    @Transactional
    public void deleteByIds(List<String> userIds) {
        List<User> users = userDomainRepository.findAllByIds(userIds);
        for (String userId : userIds) {
            Optional<User> optionalUser = users.stream().filter(u -> u.getId().equals(userId)).findFirst();
            if (optionalUser.isEmpty()) {
                throw new ResponseException(NotFoundError.USER_NOT_FOUND);
            }
            User user = optionalUser.get();
            user.deleted();
        }
        this.userDomainRepository.saveAll(users);
    }

    @Override
    public void unLock(UserUnLockRequest request) {
        List<String> usernames = request.getUsernames();
        if (!CollectionUtils.isEmpty(usernames)) {
            usernames.forEach(authFailCacheService::resetLoginFail);
        }
    }

    @Override
    public List<User> findByIds(List<String> ids) {
        return this.userDomainRepository.findAllByIds(ids);
    }

    @Override
    public PageDTO<User> autoComplete(UserSearchRequest request) {
        UserSearchQuery query = autoMapperQuery.toQuery(request);
        List<UserEntity> userEntities = userEntityRepository.search(query);
        return PageDTO.of(userEntityMapper.toDomain(userEntities),
                request.getPageIndex(),
                request.getPageSize(),
                userEntityRepository.countUser(query));
    }

    public List<ImportUserDTO> readExcelFile(MultipartFile multipartFile, HttpServletResponse httpResponse) {
        List<ImportUserDTO> importUserDTOS = new ArrayList<>();
        int rowIndex = 0;
        Map<String, Integer> hashMapEmail = new HashMap<>();
        try (XSSFWorkbook workbook = new XSSFWorkbook(multipartFile.getInputStream())) {
            Sheet sheet = workbook.getSheetAt(0);
            if (Objects.isNull(sheet)) {
                throw new ResponseException(BadRequestError.USER_INVALID);
            }
            ExcelUtils.removeEmptyRows(sheet);
            for (Row row : sheet) {
                if (rowIndex < ROW_NUMBER) {
                    rowIndex++;
                    continue;
                }
                if (rowIndex == ROW_NUMBER) {
                    rowIndex++;
                    continue;
                }
                ImportUserDTO importUserDTO = new ImportUserDTO();
                importUserDTO.setRowIndex(rowIndex);
                ImportUserCmd cmd = new ImportUserCmd();
                StringBuilder error = new StringBuilder();

                for (Cell cell : row) {
                    String value = ExcelUtils.readCellContent(cell);

                    switch (cell.getColumnIndex()) {
                        case 1:
                            if (!StrUtils.isBlank(value)) {
                                cmd.setEmployeeCode(value);
                            } else {
                                error.append(Labels.getLabels(LabelKey.COT_EMPLOYEE_CODE_NOT_EMPTY)).append(StringPool.NEW_LINE);
                            }
                            break;
                        case 2:
                            if (!StrUtils.isBlank(value) && value.length() <= 100) {
                                cmd.setFullName(value);
                            } else {
                                error.append(Labels.getLabels(LabelKey.COT_EMPLOYEE_NAME_NOT_EMPTY)).append(StringPool.NEW_LINE);
                            }
                            break;
                        case 4:
                            if (!StrUtils.isBlank(value)) {
                                DepartmentEntity departmentEntity = this.departmentEntityRepository.findDepartmentByDepartmentName(value);
                                if (Objects.nonNull(departmentEntity)) {
                                    cmd.setDepartmentId(departmentEntity.getId());
                                }
                            } else {
                                error.append(Labels.getLabels(LabelKey.COT_DEPARTMENT_NAME_NOT_EMPTY)).append(StringPool.NEW_LINE);
                            }
                            break;
                        case 6:
                            if (!StrUtils.isBlank(value)) {
                                LocalDate localDate = LocalDate.parse(value);
                                cmd.setDayOfBirth(localDate);
                                cmd.setPassword(formatPassword(localDate));
                            } else {
                                error.append(Labels.getLabels(LabelKey.COT_EMPLOYEE_DATE_BIRTH_NOT_EMPTY)).append(StringPool.NEW_LINE);
                            }
                            break;
                        case 9:
                            if (!StrUtils.isBlank(value)) {
                                if (value.equals("Nam")) {
                                    cmd.setGender(Gender.FEMALE);
                                } else if (value.equals("Nữ")) {
                                    cmd.setGender(Gender.MALE);
                                }
                            } else {
                                error.append(Labels.getLabels(LabelKey.COT_EMPLOYEE_GENDER_NOT_EMPTY)).append(StringPool.NEW_LINE);
                            }
                            break;
                        case 10:
                            String phoneRegex = ValidateConstraint.FORMAT.PHONE_NUMBER_PATTERN;
                            if (!StrUtils.isBlank(value)) {
                                if (value.matches(phoneRegex)) {
                                    cmd.setPhoneNumber(value);
                                    break;
                                }
                                error.append(Labels.getLabels(LabelKey.PHONE_FORMAT)).append(StringPool.NEW_LINE);
                            } else {
                                error.append(Labels.getLabels(LabelKey.COT_EMPLOYEE_PHONE_NOT_EMPTY)).append(StringPool.NEW_LINE);
                            }
                            break;
                        case 11:
                            String emailRegex = ValidateConstraint.FORMAT.EMAIL_PATTERN;
                            if (!StrUtils.isBlank(value)) {
                                if (value.matches(emailRegex)) {
                                    //check duplicate email trong file excel
                                    Integer count = hashMapEmail.get(value);
                                    if (Objects.isNull(count)) {
                                        hashMapEmail.put(value, 1);
                                    } else {
                                        hashMapEmail.put(value, ++count);
                                        error.append(Labels.getLabels(LabelKey.COT_EMPLOYEE_EMAIL_NOT_DUPLICATE)).append(StringPool.NEW_LINE);
                                        break;
                                    }
                                    cmd.setEmail(value);
                                    String[] userName = value.split("@");
                                    cmd.setUsername(userName[0]);
                                    break;
                                }
                                error.append(Labels.getLabels(LabelKey.EMAIL_FORMAT)).append(StringPool.NEW_LINE);
                            } else {
                                error.append(Labels.getLabels(LabelKey.COT_EMAIL_NOT_EMPTY)).append(StringPool.NEW_LINE);
                            }
                            break;
                        default:
                            break;
                    }
                }
                if (error.length() <= 0) {
                    importUserDTO.setCheck(true);
                    importUserDTO.setValue(cmd);

                } else {
                    importUserDTO.setCheck(false);
                    importUserDTO.setErrors(error);
                }
                importUserDTOS.add(importUserDTO);
                rowIndex++;
            }
        } catch (Exception e) {
            log.error("Error: ", e);
        }
        return importUserDTOS;
    }

    public String formatPassword(LocalDate localDate) {
        String[] passwords = localDate.toString().split("-");
        return passwords[0] + passwords[1] + passwords[2];
    }

    @Transactional
    @Override
    public void importUser(MultipartFile file, HttpServletResponse response) {
        List<User> createUsers = new ArrayList<>();
        List<User> updateUsers = new ArrayList<>();
        List<ImportUserDTO> importUserDTOS = readExcelFile(file, response);
        boolean checkTotalRecordSuccess = true;
        List<String> emails = importUserDTOS.stream()
                .filter(it -> Boolean.TRUE.equals(it.getCheck()))
                .map(item -> item.getValue().getEmail().toLowerCase())
                .collect(Collectors.toList());
        List<UserEntity> userEntities = this.userEntityRepository.findAllByEmails(emails);
        for (ImportUserDTO importUserDTO : importUserDTOS) {
            if (importUserDTO.getCheck()) {
                Optional<UserEntity> userEntity = userEntities.stream()
                        .filter(it -> Objects.equals(importUserDTO.getValue().getEmail(), it.getEmail()))
                        .findFirst();
                if (userEntity.isPresent()) {
                    //update
                    User user = this.userEntityMapper.toDomain(userEntity.get());
                    user.updateImport(importUserDTO.getValue());
                    updateUsers.add(user);
                } else {
                    //create
                    User user = new User(importUserDTO.getValue());
                    createUsers.add(user);
                    UserInternalCreateRequest userInternalCreateRequest = autoMapper.from(importUserDTO.getValue());
                    this.ldapService.createUserLdap(this.autoMapper.fromInternal(userInternalCreateRequest));
                }
            } else {
                checkTotalRecordSuccess = false;
            }
        }
        if (!CollectionUtils.isEmpty(createUsers) && Boolean.TRUE.equals(checkTotalRecordSuccess)) {
            this.userDomainRepository.saveAll(createUsers);
        } else if (!CollectionUtils.isEmpty(updateUsers) && Boolean.TRUE.equals(checkTotalRecordSuccess)) {
            this.userDomainRepository.saveAll(updateUsers);
        }
        this.getResultExcel(file, importUserDTOS, response);
    }

    public void getResultExcel(MultipartFile file, List<ImportUserDTO> importUserDTOS, HttpServletResponse response) {
        try {
            XSSFWorkbook workbook = new XSSFWorkbook(file.getInputStream());
            Sheet sheet = workbook.getSheetAt(0);
            CellStyle errorStyle = ExcelUtils.createErrorCellStyle(workbook);
            boolean status = true;
            if (Objects.isNull(sheet)) {
                throw new ResponseException(BadRequestError.USER_INVALID);
            }
            for (ImportUserDTO importUserDTO : importUserDTOS) {
                Row row = sheet.getRow(importUserDTO.getRowIndex());
                if (Boolean.FALSE.equals(importUserDTO.getCheck()) && Objects.nonNull(importUserDTO.getErrors())) {
                    status = false;
                    ExcelUtils.createCell(row, 12, errorStyle, importUserDTO.getErrors().toString());
                    ExcelUtils.createCell(row, 13, errorStyle, Labels.getLabels(LabelKey.MESSAGE_FAIL));
                }
            }
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
            String fileNameResponse = String.format("users_result_%s", DateUtils.format(new Date(), "yyyyMMddHHmmss"));
            response.addHeader("Content-Disposition", "attachment; filename=" + fileNameResponse + StringPool.XLSX);
            response.addHeader("Access-Control-Allow-Headers", "Content-Type");
            response.addHeader("Access-Control-Expose-Headers", "Content-Disposition, status");
            response.setHeader("status", Boolean.toString(status));
            OutputStream outputStream = response.getOutputStream();
            workbook.write(outputStream);
            workbook.close();
            outputStream.close();
        } catch (IOException e) {
            log.error("Error: ", e);
        }
    }

    @Override
    public void syncUserLdap() {
        this.syncService.syncUserLdap();
    }

    @Override
    public List<User> findByDepartmentIds(List<String> ids) {
        List<UserEntity> userEntities = userEntityRepository.findAllUserByDepartmentIds(ids, UserStatus.ACTIVE);
        return userEntityMapper.toDomain(userEntities);
    }


    public void validateBaseInformation(UserInternalCreateRequest request) {
        // valid user by phoneNumber
        Optional<UserEntity> userEntityByPhone = userEntityRepository
                .findByPhoneNumber(request.getPhoneNumber());
        if (userEntityByPhone.isPresent()) {
            throw new ResponseException(BadRequestError.USER_PHONE_NUMBER_EXITED);
        }

        // valid user by email
        Optional<UserEntity> userEntityByEmail = userEntityRepository.findByEmail(request.getEmail());
        if (userEntityByEmail.isPresent()) {
            throw new ResponseException(BadRequestError.USER_EMAIL_EXITED);
        }

        // valid user by employee code
        if (Objects.nonNull(request.getEmployeeCode())) {
            Optional<UserEntity> userEntityEmployeeCode = userEntityRepository.findByEmployeeCode(request.getEmployeeCode());
            if (userEntityEmployeeCode.isPresent()) {
                throw new ResponseException(BadRequestError.USER_EMPLOYEE_CODE_EXITED);
            }
        }

        // valid username
        if (userEntityRepository.findByUsername(request.getUsername()).isPresent()) {
            throw new ResponseException(BadRequestError.USER_USERNAME_EXITED);
        }

        // valid password
        if (!StrUtils.isBlank(request.getPassword()) && !StringUtil.validatePassword(request.getPassword())) {
            throw new ResponseException(BadRequestError.PASSWORD_NOT_STRONG);
        }

        if (!StrUtils.isBlank(request.getPassword())
                && !StrUtils.isBlank(request.getRepeatPassword())
                && !Objects.equals(request.getPassword(), request.getRepeatPassword())) {
            throw new ResponseException(BadRequestError.REPEAT_PASSWORD_DOES_NOT_MATCH);
        }

        if (Objects.nonNull(request.getAvatarFileId())) {
            Response<FileDTO> responseFile = storageClient.findById(request.getAvatarFileId());
            if (!responseFile.isSuccess()) {
                throw new ResponseException(BadRequestError.FILE_NOT_EXISTED);
            }
        }
    }

    @Override
    @Transactional
    public void active(String userId) {
        User user = this.userDomainRepository.getById(userId);
        user.active();
        this.userDomainRepository.save(user);
    }

    @Override
    @Transactional
    public void inactive(String userId) {
        User user = this.userDomainRepository.getById(userId);
        user.inactive();
        this.userDomainRepository.save(user);
    }

    @Override
    public User getUserById(String userId) {
        User user = this.userDomainRepository.getById(userId);

        // enrich roles
        List<String> roleIds = user.getUserRoles().stream()
                .map(UserRole::getRoleId)
                .collect(Collectors.toList());
        List<Role> roles = roleEntityRepository.findAllByIds(roleIds).stream()
                .filter(r -> Objects.equals(RoleStatus.ACTIVE, r.getStatus()))
                .map(roleEntityMapper::toDomain)
                .collect(Collectors.toList());
        user.enrichRoles(roles);

        if (Objects.nonNull(user.getAvatarFileId())) {
            Response<FileDTO> responseFile = storageClient.findById(user.getAvatarFileId());
            if (responseFile.isSuccess() && Objects.nonNull(responseFile.getData())) {
                user.enrichViewUrlFile(responseFile.getData().getViewUrl());
            }
        }

        return user;
    }

    @Override
    @Transactional
    public User changePassword(String userId, UserChangePasswordRequest request) {
        User user = this.userDomainRepository.getById(userId);
        String newEncodedPassword = this.passwordEncoder.encode(request.getNewPassword());
        if (Objects.equals(user.getAuthenticationType(), AuthenticationType.LDAP)) {
            if (!this.ldapService.resetPassUserLdap(user.getFullName(), this.autoMapper.from(request))) {
                throw new ResponseException(BadRequestError.ACCOUNT_LDAP_CAN_NOT_CHANGE_PASSWORD);
            }
        }
        user.changePassword(newEncodedPassword);
        this.userDomainRepository.save(user);
        return user;
    }

    @Override
    public void downloadExcelTemplateFromResource(HttpServletResponse response) {
        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        String fileName = this.templateProperties.getUser().getTemplateFileName();
        String fileNameTimestamp = fileName + StringPool.UNDERLINE + DateUtils.format(new Date(), "yyyyMMddHHmmss") + StringPool.XLSX;
        response.addHeader("Content-Disposition", "attachment; filename=" + fileNameTimestamp);
        response.addHeader("X-Action-Mesage", fileNameTimestamp);
        response.addHeader("Access-Control-Allow-Headers", "Content-Type");
        response.addHeader("Access-Control-Expose-Headers", "Content-Disposition");
        try {
            InputStream inputStream = new ClassPathResource(
                    this.templateProperties.getUser().getTemplateFileExcel()
                            + StringPool.SLASH
                            + this.templateProperties.getUser().getTemplateFileName()
                            + StringPool.XLSX)
                    .getInputStream();
            OutputStream outputStream = response.getOutputStream();
            Context context = new Context();
            context.putVar("departments", this.departmentEntityMapper.toDomain(this.departmentEntityRepository.findAllActivated()));
            JxlsHelper.getInstance().processTemplate(inputStream, outputStream, context);
            response.flushBuffer();
        } catch (FileNotFoundException e) {
            throw new ResponseException(NotFoundError.FILE_NOT_FOUND);
        } catch (Exception e) {
            log.error("", e);
        }
    }
}

