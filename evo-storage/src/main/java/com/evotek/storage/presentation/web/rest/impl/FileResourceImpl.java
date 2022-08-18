package com.evotek.storage.presentation.web.rest.impl;

import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.error.NotFoundError;
import com.evotek.common.exception.ResponseException;
import com.evotek.storage.application.dto.request.FileSearchRequest;
import com.evotek.storage.application.dto.response.FileViewResponse;
import com.evotek.storage.application.service.FileService;
import com.evotek.storage.domain.File;
import com.evotek.storage.infrastructure.support.storage.StorageService;
import com.evotek.storage.presentation.web.rest.FileResource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;

@Slf4j
@RestController
public class FileResourceImpl implements FileResource {

    private final FileService fileService;
    private final StorageService storageService;

    @Value("${app.storage.ios-app-file-path}")
    private String iosAppFilePath;

    public FileResourceImpl(FileService fileService, StorageService storageService) {
        this.fileService = fileService;
        this.storageService = storageService;
    }

    @Override
    public Response<List<File>> fileUploads(List<MultipartFile> files, String ownerId, String ownerType) {
        return Response.of(this.fileService.createFile(files, ownerId, ownerType));
    }

    @Override
    public Response<File> fileUpload(MultipartFile file, String ownerId, String ownerType) {
        return Response.of(this.fileService.createFile(file, ownerId, ownerType));
    }


    @Override
    public ResponseEntity<Mono<InputStreamResource>> downloadFileMono(String fileId) {

        File file = fileService.ensureExisted(fileId);
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + file.getOriginalName())
                .body(this.storageService.download(file.getPath()));
    }

    @Override
    public ResponseEntity<InputStreamResource> downloadIosAppFile() {
        HttpHeaders responseHeader = new HttpHeaders();
        InputStreamResource inputStreamResource = this.storageService.getFileInputStream(iosAppFilePath);
        responseHeader.setContentType(MediaType.APPLICATION_OCTET_STREAM);
        responseHeader.set("Content-disposition", "attachment; filename=" + FilenameUtils.getName(iosAppFilePath));
        return new ResponseEntity<>(inputStreamResource, responseHeader, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<?> viewFileImage(String fileId) {
        FileViewResponse fileViewResponse = this.fileService.getFileView(fileId);
        HttpHeaders responseHeader = new HttpHeaders();
        try {
            if (Objects.nonNull(fileViewResponse.getFile())  && fileViewResponse.getFile().isImage()) {
                responseHeader.setContentType(MediaType.parseMediaType(fileViewResponse.getFile().getType()));
            } else {
                responseHeader.setContentType(MediaType.APPLICATION_OCTET_STREAM);
                responseHeader.set("Content-disposition", "attachment; filename=" + fileViewResponse.getFile().getOriginalName());
            }
        } catch (Exception e) {
            log.error("Error parsing media type", e);
            responseHeader.setContentType(MediaType.APPLICATION_OCTET_STREAM);
        }
        return new ResponseEntity<>(fileViewResponse.getInputStreamResource(), responseHeader, HttpStatus.OK);
    }

    @Override
    public PagingResponse<File> search(FileSearchRequest fileSearchRequest) {
        return PagingResponse.of(fileService.search(fileSearchRequest));
    }

    @Override
    public Response<File> deleteById(String fileId) {
        return Response.of(this.fileService.deleteFileById(fileId));
    }

    @Override
    public Response<File> findById(String fileId) {
        return Response.of(this.fileService.ensureExisted(fileId));
    }

    @Override
    public Response<List<File>> findByIds(FindByIdsRequest request) {
        return Response.of(this.fileService.findByIds(request.getIds()));
    }

    @Override
    public Response<List<File>> checkServerAvailable() {
        return Response.ok();
    }

    @Override
    public Response<List<File>> publicFileUploads(List<MultipartFile> files) {
        return Response.of(this.fileService.createFile(files, null, null));
    }

    @Override
    public Response<File> publicFileUpload(MultipartFile file) {
        return Response.of(this.fileService.createFile(file, null, null));
    }
}
