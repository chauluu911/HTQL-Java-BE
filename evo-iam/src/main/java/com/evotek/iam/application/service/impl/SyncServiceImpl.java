package com.evotek.iam.application.service.impl;

import com.evotek.common.ldapcore.dto.response.UserLdapResponse;
import com.evotek.common.ldapcore.service.LDAPService;
import com.evotek.iam.application.service.SyncService;
import com.evotek.iam.domain.User;
import com.evotek.iam.infrastructure.persistence.entity.UserEntity;
import com.evotek.iam.infrastructure.persistence.mapper.UserEntityMapper;
import com.evotek.iam.infrastructure.persistence.repository.UserEntityRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@Slf4j
public class SyncServiceImpl implements SyncService {

    private final LDAPService ldapService;

    private final UserEntityRepository userEntityRepository;

    private final PasswordEncoder passwordEncoder;

    private final UserEntityMapper userEntityMapper;

    public SyncServiceImpl(LDAPService ldapService, UserEntityRepository userEntityRepository, PasswordEncoder passwordEncoder, UserEntityMapper userEntityMapper) {
        this.ldapService = ldapService;
        this.userEntityRepository = userEntityRepository;
        this.passwordEncoder = passwordEncoder;
        this.userEntityMapper = userEntityMapper;
    }

    @Override
    public void syncUserLdap() {
        List<UserLdapResponse> userLdapResponses = ldapService.getAllUserInLdap();
        if (CollectionUtils.isEmpty(userLdapResponses)) {
            log.error("Cant get user from LDAP");
            return;
        }

        List<UserEntity> employees = userEntityRepository.findAllUserActive();
        List<String> userLdapNotExistInSys = userLdapResponses.stream()
                .map(UserLdapResponse::getUserName)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        // lay nhung tai khoan user ldap chua duoc tao trong db
        userLdapNotExistInSys.removeAll(employees.stream().map(UserEntity::getUsername).collect(Collectors.toList()));

        // create user from account ldap
        List<UserLdapResponse> userLdapNeedSync = userLdapResponses.stream()
                .filter(userRes -> userLdapNotExistInSys.contains(userRes.getUserName()))
                .collect(Collectors.toList());

        // create and save to database
        createListLdapUser(userLdapNeedSync);
    }

    private void createListLdapUser(List<UserLdapResponse> userLdapNeedSync) {
        List<User> users = new ArrayList<>();
        for (UserLdapResponse userLdap : userLdapNeedSync) {
            User user = createLdapUser(userLdap);
            users.add(user);
        }
        List<UserEntity> userEntities = userEntityMapper.toEntity(users);
        userEntityRepository.saveAll(userEntities);
    }

    public User createLdapUser(UserLdapResponse userLdapResponse) {
        String encodedPassword = this.passwordEncoder.encode(userLdapResponse.getUserPrincipalName());
        userLdapResponse.setMail(encodedPassword);
        return new User(userLdapResponse);
    }

}

