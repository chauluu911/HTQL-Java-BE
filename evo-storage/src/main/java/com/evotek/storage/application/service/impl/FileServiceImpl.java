package com.evotek.storage.application.service.impl;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.SecurityUtils;
import com.evotek.storage.application.config.StorageProperties;
import com.evotek.storage.application.dto.request.FileSearchRequest;
import com.evotek.storage.application.dto.response.FileViewResponse;
import com.evotek.storage.application.mapper.AutoMapperQuery;
import com.evotek.storage.application.service.FileService;
import com.evotek.storage.domain.File;
import com.evotek.storage.domain.command.FileCreateCmd;
import com.evotek.storage.domain.query.FileSearchQuery;
import com.evotek.storage.domain.repositoty.FileDomainRepository;
import com.evotek.storage.infrastructure.persistence.entity.FileEntity;
import com.evotek.storage.infrastructure.persistence.mapper.FileEntityMapper;
import com.evotek.storage.infrastructure.persistence.repository.FileEntityRepository;
import com.evotek.storage.infrastructure.support.exception.BadRequestError;
import com.evotek.storage.infrastructure.support.storage.StorageService;
import com.evotek.storage.infrastructure.support.util.Const;
import com.evotek.storage.infrastructure.support.util.MD5Util;
import com.evotek.storage.infrastructure.support.util.StringUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.InputStreamResource;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.transaction.Transactional;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static com.evotek.common.util.StringPool.SLASH;

@Service
@Slf4j
@RequiredArgsConstructor
public class FileServiceImpl implements FileService {

    private final FileEntityRepository fileEntityRepository;
    private final FileDomainRepository fileDomainRepository;
    private final FileEntityMapper fileEntityMapper;
    private final StorageService storageService;
    private final StorageProperties properties;
    private final AutoMapperQuery fileAutoMapper;

    /*
     * @author: huynq
     * @since: 11/15/2021 6:00 AM
     * @description: Check file existed
     * @update:
     */
    @Override
    public File ensureExisted(String id) {
        return this.fileDomainRepository.getById(id);
    }

    @Override
    public List<File> findByIds(List<String> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return new ArrayList<>();
        }
        List<FileEntity> fileEntities = this.fileEntityRepository.findByIds(ids);
        if (CollectionUtils.isEmpty(fileEntities)) {
            return new ArrayList<>();
        }

        List<File> files = this.fileEntityMapper.toDomain(fileEntities);
        for (File file : files) {
            file.enrichPublicUrl(this.properties.getPublicStorageUrl());
        }
        return files;
    }

    @Override
    public PageDTO<File> search(FileSearchRequest request) {
        FileSearchQuery query = this.fileAutoMapper.from(request);
        return new PageDTO<>(fileEntityMapper.toDomain(this.fileEntityRepository.search(query)),
                request.getPageIndex(), request.getPageSize(), this.fileEntityRepository.count(query));
    }

    @Override
    @Transactional
    public List<File> createFile(List<MultipartFile> files, String ownerId, String ownerType) {
        validateFile(files);
        if ((!StringUtils.hasLength(ownerId)) || (!StringUtils.hasLength(ownerType))) {
            ownerType = Const.DEFAULT_OWNER_TYPE;
            Optional<String> optionalUserId = SecurityUtils.getCurrentUserLoginId();
            ownerId = optionalUserId.orElse(Const.DEFAULT_OWNER);
        }

        List<File> fileUploads = new ArrayList<>();
        String finalOwnerType = ownerType;
        String finalOwnerId = ownerId;
        files.forEach(item -> {
            File fileUpload = this.uploadFile(item, finalOwnerId, finalOwnerType);
            fileUploads.add(fileUpload);
        });
        // save list file in db and return list file
        return this.fileDomainRepository.saveAll(fileUploads);
    }

    @Override
    @Transactional
    public File createFile(MultipartFile file, String ownerId, String ownerType) {
        validateFile(List.of(file));

        if ((!StringUtils.hasLength(ownerId)) || (!StringUtils.hasLength(ownerType))) {
            ownerType = Const.DEFAULT_OWNER_TYPE;
            Optional<String> optionalUserId = SecurityUtils.getCurrentUserLoginId();
            ownerId = optionalUserId.orElse(Const.DEFAULT_OWNER);
        }

        File fileUpload = this.uploadFile(file, ownerId, ownerType);
        return this.fileDomainRepository.save(fileUpload);
    }

    private File uploadFile(MultipartFile file, String ownerId, String ownerType) {
        String originalName = StringUtils.cleanPath(Objects.requireNonNull(file.getOriginalFilename()));
        String path;
        long version = 1L;
        String md5Hashed = this.getHashedFileMd5(file);
        String fileName =
                File.genNewFileName(
                        StringUtil.getSafeFileName(Objects.requireNonNull(file.getOriginalFilename())), md5Hashed);
        log.info("object name {}", fileName);
        try {
            String folder = SLASH + ownerType + this.storageService.getFolderPath(fileName);
            path = this.storageService.store(file, folder, fileName);
        } catch (Exception e) {
            throw new ResponseException(BadRequestError.FILE_UPLOAD_INVALID);
        }
        // check version file
        Optional<FileEntity> findExisted = this.fileEntityRepository
                .findFirstByOriginalNameAndAndOwnerIdAndOwnerType(originalName, ownerId, ownerType);
        if (findExisted.isPresent()) {
            version = findExisted.get().getVersion() + 1L;
        }
//        String mimeType = file.getContentType();
        // save file in db and return file
        FileCreateCmd fileCmd = FileCreateCmd.builder()
                .originalName(originalName)
                .path(path)
                .size(file.getSize())
                .type(file.getContentType())
                .ownerId(ownerId)
                .ownerType(ownerType)
                .hashed(md5Hashed)
                .version(version)
                .build();

        File fileUpload = new File(fileCmd);
        fileUpload.enrichPublicUrl(this.properties.getPublicStorageUrl());
        return fileUpload;
    }

    private String getHashedFileMd5(MultipartFile file) {
        String md5Hashed;
        try {
            md5Hashed = MD5Util.MD5(file.getBytes());
        } catch (IOException e) {
            log.error("Gen MD5 error", e);
            throw new ResponseException(BadRequestError.FILE_UPLOAD_INVALID);
        }
        return md5Hashed;
    }

    @Override
    public File deleteFileById(String id) {
        File file = this.ensureExisted(id);
        file.deleted();
        return this.fileDomainRepository.save(file);
    }

    /*
     * @author: huynq
     * @since: 11/15/2021 1:53 PM
     * @description:  get File resource
     * @update:
     */
    @Override
    public FileViewResponse getFileView(String fileId) {
        File file = this.ensureExisted(fileId);
        String path = file.getPath();
        InputStreamResource inputStreamResource = this.storageService.getFileInputStream(path);
        return new FileViewResponse(file, inputStreamResource);
    }


    private void validateFile(List<MultipartFile> multipartFiles) {
        if (CollectionUtils.isEmpty(multipartFiles)) {
            throw new ResponseException(BadRequestError.FILE_UPLOAD_INVALID);
        }
//        var tika = new Tika();
        for (MultipartFile file : multipartFiles) {
            log.info("file type {}", file.getContentType());
            if (Objects.isNull(file.getOriginalFilename()) || "".equals(file.getOriginalFilename())) {
                throw new ResponseException(BadRequestError.FILE_UPLOAD_INVALID);
            }

            if (!CollectionUtils.isEmpty(properties.getWhitelistMimeTypes()) && properties.getWhitelistMimeTypes()
                    .stream().noneMatch(extension -> Objects.equals(extension.trim(), file.getContentType()))) {

                throw new ResponseException(BadRequestError.EXTENSION_FILE_UPLOAD_INVALID);
            }
        }
    }
}
