package com.evotek.common.ldapData.application.config;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.ldapData.infrastructure.support.exception.BadRequestError;
import com.evotek.common.ldapcore.config.LDAPProperties;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.ldap.repository.config.EnableLdapRepositories;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.ldap.core.support.DefaultDirObjectFactory;
import org.springframework.ldap.core.support.LdapContextSource;

import javax.net.ssl.*;
import java.net.Socket;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

@Slf4j
@Configuration
@EnableLdapRepositories(basePackages = "com.evotek.common.ldapData.*")
@EnableConfigurationProperties(LDAPProperties.class)
public class LdapConnectionConfig {
    private final LDAPProperties ldapProperties;

    public LdapConnectionConfig(LDAPProperties ldapProperties) {
        this.ldapProperties = ldapProperties;
    }

    @Bean
    public LdapContextSource ldapContextSource() {
        LdapContextSource ldapContextSource = new LdapContextSource();
        ldapContextSource.setUrl(this.ldapProperties.getUrls());
        ldapContextSource.setUserDn(this.ldapProperties.getUsername());
        ldapContextSource.setPassword(this.ldapProperties.getPassword());
        ldapContextSource.setDirObjectFactory(DefaultDirObjectFactory.class);
        ldapContextSource.setBase(this.ldapProperties.getBase());
        try {
            setSsl();
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        } catch (KeyManagementException e) {
            throw new RuntimeException(e);
        }
        ldapContextSource.setCacheEnvironmentProperties(false);
        ldapContextSource.setPooled(false);
        ldapContextSource.setReferral("follow");
        try {
            ldapContextSource.afterPropertiesSet();
        } catch (Exception e) {
            throw new ResponseException(BadRequestError.NOT_CREATE_CONTEXT_SOURCE_LDAP);
        }
        return ldapContextSource;
    }

    @Bean
    public LdapTemplate ldapTemplate() {
        return new LdapTemplate(ldapContextSource());
    }

    private void setSsl() throws NoSuchAlgorithmException, KeyManagementException {
        SSLContext ctx = SSLContext.getInstance("TLS");
        X509TrustManager tm;

        if (true) {
            tm = new X509ExtendedTrustManager() {
                @Override
                public void checkClientTrusted(X509Certificate[] x509Certificates, String s, Socket socket) throws CertificateException {

                }

                @Override
                public void checkServerTrusted(X509Certificate[] x509Certificates, String s, Socket socket) throws CertificateException {

                }

                @Override
                public void checkClientTrusted(X509Certificate[] x509Certificates, String s, SSLEngine sslEngine) throws CertificateException {

                }

                @Override
                public void checkServerTrusted(X509Certificate[] x509Certificates, String s, SSLEngine sslEngine) throws CertificateException {

                }

                @Override
                public void checkClientTrusted(X509Certificate[] x509Certificates, String s) throws CertificateException {

                }

                @Override
                public void checkServerTrusted(X509Certificate[] x509Certificates, String s) throws CertificateException {

                }

                @Override
                public X509Certificate[] getAcceptedIssuers() {
                    return new X509Certificate[0];
                }
            };
        } else {
            tm = new X509TrustManager() {

                public void checkClientTrusted(X509Certificate[] xcs, String string) throws CertificateException {
                }

                public void checkServerTrusted(X509Certificate[] xcs, String string) throws CertificateException {
                }

                public X509Certificate[] getAcceptedIssuers() {
                    return new X509Certificate[0];
                }
            };
        }

        ctx.init(null, new TrustManager[]{tm}, null);
        SSLContext.setDefault(ctx);
    }
}
