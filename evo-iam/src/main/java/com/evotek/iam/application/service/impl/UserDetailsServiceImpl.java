package com.evotek.iam.application.service.impl;

import com.evotek.common.error.AuthenticationError;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.security.AuthorityService;
import com.evotek.iam.infrastructure.persistence.entity.UserEntity;
import com.evotek.iam.infrastructure.persistence.repository.UserEntityRepository;
import com.evotek.iam.infrastructure.support.exception.NotFoundError;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class UserDetailsServiceImpl implements UserDetailsService {

    private final UserEntityRepository userEntityRepository;
    private final AuthorityService authorityService;

    public UserDetailsServiceImpl(UserEntityRepository userEntityRepository, AuthorityService authorityService) {
        this.userEntityRepository = userEntityRepository;
        this.authorityService = authorityService;
    }

    @Override
    public UserDetails loadUserByUsername(String s) throws UsernameNotFoundException {
        if (!StringUtils.hasLength(s)) {
            throw new ResponseException(AuthenticationError.UNAUTHORISED);
        }
        Optional<UserEntity> optionalUserEntity = this.userEntityRepository.findByUsername(s);
        if (optionalUserEntity.isPresent()) {
            UserEntity userEntity = optionalUserEntity.get();
            return enrichUserInfo(userEntity);
        } else {
            throw new ResponseException(NotFoundError.USER_NOT_FOUND.getMessage(), NotFoundError.USER_NOT_FOUND, s);
        }
    }

    private User enrichUserInfo(UserEntity userEntity) {
        Set<GrantedAuthority> authorities = new HashSet<>();
        List<String> listAuthorities = authorityService.getUserAuthority(userEntity.getId()).getGrantedPermissions();
        authorities = listAuthorities.isEmpty() ? authorities
                : listAuthorities.stream().map(SimpleGrantedAuthority::new).collect(Collectors.toSet());
        return new User(userEntity.getUsername(), userEntity.getPassword(), authorities);
    }
}
