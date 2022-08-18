package com.evotek.common.ldapData.application.service.iml;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.ldapData.application.dto.request.UserLdapCreateOrUpdateRequest;
import com.evotek.common.ldapData.application.service.UserLdapService;
import com.evotek.common.ldapData.domain.UserLdap;
import com.evotek.common.ldapData.domain.command.UserCreateOrUpdateCmd;
import com.evotek.common.ldapData.infrastructure.presistence.entry.UserLdapEntry;
import com.evotek.common.ldapData.infrastructure.presistence.mapper.AutoMapper;
import com.evotek.common.ldapData.infrastructure.presistence.mapper.UserLdapEntryMapper;
import com.evotek.common.ldapData.infrastructure.presistence.repository.UserLdapRepository;
import com.evotek.common.ldapData.infrastructure.support.exception.BadRequestError;
import com.evotek.common.ldapData.infrastructure.support.exception.NotFoundError;
import com.evotek.common.ldapData.infrastructure.support.util.Const;
import com.evotek.common.ldapcore.config.LDAPProperties;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.ldap.query.LdapQuery;
import org.springframework.ldap.query.SearchScope;
import org.springframework.ldap.support.LdapNameBuilder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.naming.Name;
import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.BasicAttribute;
import javax.naming.directory.DirContext;
import javax.naming.directory.ModificationItem;
import java.util.List;
import java.util.Optional;

import static org.springframework.ldap.query.LdapQueryBuilder.query;

@Service
@Slf4j
@EnableConfigurationProperties(LDAPProperties.class)
public class UserLdapServiceIml implements UserLdapService {
    private final LdapTemplate ldapTemplate;
    private final UserLdapRepository userLdapRepository;
    private final UserLdapEntryMapper userLdapEntryMapper;
    private final AutoMapper autoMapper;

    private final LDAPProperties ldapProperties;

    public UserLdapServiceIml(LdapTemplate ldapTemplate, UserLdapRepository userLdapRepository, UserLdapEntryMapper userLdapEntryMapper, AutoMapper autoMapper, LDAPProperties ldapProperties) {
        this.ldapTemplate = ldapTemplate;
        this.userLdapRepository = userLdapRepository;
        this.userLdapEntryMapper = userLdapEntryMapper;
        this.autoMapper = autoMapper;
        this.ldapProperties = ldapProperties;
    }

    @Override
    public List<UserLdap> getAllUserInLdap() {
        List<UserLdapEntry> userLdapEntries = this.userLdapRepository.findAll();
        return userLdapEntryMapper.toDomain(userLdapEntries);
    }

    @Override
    public UserLdap findUserLdapById(String id) {
        Optional<UserLdapEntry> userLdapEntryOptional = this.userLdapRepository.findById(buildName(id));
        if (userLdapEntryOptional.isEmpty()) {
            throw new ResponseException(NotFoundError.USER_NOT_FOUND);
        }
        return this.userLdapEntryMapper.toDomain(userLdapEntryOptional.get());
    }

    @Override
    @Transactional
    public void resetPassword(String username, String password) {
        LdapQuery query = query()
                .searchScope(SearchScope.SUBTREE)
                .where("sAMAccountName").is(username);
        Optional<UserLdapEntry> userOptional = this.userLdapRepository.findOne(query);
        if (userOptional.isEmpty()) {
            throw new ResponseException(NotFoundError.USER_NOT_FOUND.getMessage(), NotFoundError.USER_NOT_FOUND, username);
        }
        UserLdapEntry user = userOptional.get();
        user.setNew(false);
        Attribute resetPass = new BasicAttribute("unicodePwd", encodePassword(password));
        ModificationItem item = new ModificationItem(DirContext.REPLACE_ATTRIBUTE, resetPass);
        try {
            this.ldapTemplate.getContextSource().getReadWriteContext().modifyAttributes(user.getId(), new ModificationItem[]{item});
        } catch (NamingException e) {
            throw new ResponseException(BadRequestError.NOT_UPDATE_CONTEXT_SOURCE_LDAP);
        }
        this.userLdapRepository.save(user);
    }

    private byte[] encodePassword(String password) {
        String quotedPassword = "\"" + password + "\"";
        char[] unicodePwd = quotedPassword.toCharArray();
        byte[] pwdArray = new byte[unicodePwd.length * 2];
        for (int i = 0; i < unicodePwd.length; i++) {
            pwdArray[i * 2 + 1] = (byte) (unicodePwd[i] >>> 8);
            pwdArray[i * 2 + 0] = (byte) (unicodePwd[i] & 0xff);
        }
        return pwdArray;
    }

    @Override
    @Transactional
    public UserLdap create(UserLdapCreateOrUpdateRequest request) {
        UserCreateOrUpdateCmd cmd = this.autoMapper.from(request);
        UserLdap userLdap = new UserLdap(cmd, this.ldapProperties);
        UserLdapEntry userNew = this.userLdapEntryMapper.toEntity(userLdap);
        userNew.setNew(true);
        this.userLdapRepository.save(userNew);
        return userLdap;
    }

    @Override
    public UserLdap update(String idUser, UserLdapCreateOrUpdateRequest request) {
        Optional<UserLdapEntry> userLdapEntryOptional = this.userLdapRepository.findById(buildName(idUser));
        if (userLdapEntryOptional.isEmpty()) {
            throw new ResponseException(NotFoundError.USER_NOT_FOUND);
        }
        UserCreateOrUpdateCmd cmd = this.autoMapper.from(request);
        UserLdap userLdap = this.userLdapEntryMapper.toDomain(userLdapEntryOptional.get());
        userLdap.update(cmd, this.ldapProperties);
        UserLdapEntry userNew = this.userLdapEntryMapper.toEntity(userLdap);
        userNew.setNew(false);
        this.userLdapRepository.save(userNew);
        return userLdap;
    }

    @Override
    public Boolean authenticate(String username, String password) {
        LdapQuery query = query()
                .searchScope(SearchScope.SUBTREE)
                .where("sAMAccountName").is(username);
        Optional<UserLdapEntry> userLdapEntryOptional = this.userLdapRepository.findOne(query);
        if (userLdapEntryOptional.isEmpty()) {
            return false;
        } else {
            try {
                this.ldapTemplate.authenticate(query, password);
                return true;
            } catch (Exception e) {
                return false;
            }
        }
    }

    public Name buildName(String id) {
        return LdapNameBuilder.newInstance()
                .add(Const.COMMON_NAME, id).build();
    }
}
