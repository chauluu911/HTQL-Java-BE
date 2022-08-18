/*
 * MD5Util.java
 *
 * Copyright (C) 2021 by Vinsmart. All right reserved.
 * This software is the confidential and proprietary information of Vinsmart
 */
package com.evotek.storage.infrastructure.support.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileInputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * The Class MD5Util.
 */
public class MD5Util {

    /**
     * The Constant log.
     */
    private final static Logger log = LoggerFactory.getLogger(MD5Util.class);

    public static String MD5(byte[] file) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");

            byte[] mdBytes = md.digest(file);
            StringBuilder sb = new StringBuilder();
            for (byte mdByte : mdBytes) {
                sb.append(Integer.toString((mdByte & 0xff) + 0x100, 16).substring(1));
            }

            return sb.toString();
        } catch (NoSuchAlgorithmException ex) {
            log.error("get MD5 error: ", ex);
            return "";
        }
    }

    public static String MD5(String fileUrl) {
        FileInputStream fis = null;
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            fis = new FileInputStream(fileUrl);

            byte[] dataBytes = new byte[1024];

            int nRead;
            while ((nRead = fis.read(dataBytes)) != -1) {
                md.update(dataBytes, 0, nRead);
            }
            byte[] mdBytes = md.digest();

            // convert the byte to hex format method 1
            StringBuilder sb = new StringBuilder();
            for (byte mdByte : mdBytes) {
                sb.append(Integer.toString((mdByte & 0xff) + 0x100, 16).substring(1));
            }

            return sb.toString();
        } catch (IOException | NoSuchAlgorithmException ex) {
            log.error("get MD5 error", ex);
            return "";
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (IOException ex) {
                    log.error("get MD5 error: ", ex);
                }
            }
        }
    }

    private static String bytesToHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder();
        for (byte aByte : bytes) {
            sb.append(Integer.toString((aByte & 0xff) + 0x100, 16).substring(1));
        }
        return sb.toString();
    }
}
