package com.evotek.iam.application.service;

public interface SyncService {
    /**
     * đồng bộ thông tin cơ bản của user
     * tù ldap về hệ thống
     */
    void syncUserLdap();
}

