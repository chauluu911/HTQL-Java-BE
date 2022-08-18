package com.evotek.storage.presentation.web.rest;

import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.PagingResponse;
import com.evotek.common.dto.response.Response;
import com.evotek.common.validator.ValidatePaging;
import com.evotek.storage.application.dto.request.FileSearchRequest;
import com.evotek.storage.domain.File;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import reactor.core.publisher.Mono;

import java.util.List;

@Api(tags = "File Resource")
@RequestMapping("/api")
@Validated
public interface FileResource {

    @ApiOperation(value = "Upload multi file")
    @PostMapping("/files/uploads")
    @PreAuthorize("hasPermission(null, 'file:create')")
    Response<List<File>> fileUploads(@RequestPart("files") List<MultipartFile> files,
                                     @RequestParam(value = "ownerId", required = false) String ownerId,
                                     @RequestParam(value = "ownerType", required = false) String ownerType);

    @ApiOperation(value = "Upload single file")
    @PostMapping("/files/upload")
    @PreAuthorize("hasPermission(null, 'file:create')")
    Response<File> fileUpload(@RequestPart(name = "file") MultipartFile file,
                              @RequestParam(value = "ownerId", required = false) String ownerId,
                              @RequestParam(value = "ownerType", required = false) String ownerType);


//    @ApiOperation(value = "Download File")
//    @GetMapping("/files/{fileId}/download")
//    ResponseEntity<InputStreamResource> downloadFileById(@PathVariable("fileId") String fileId);

    @ApiOperation(value = "Download File")
    @GetMapping("/files/{fileId}/download")
    ResponseEntity<Mono<InputStreamResource>> downloadFileMono(@PathVariable("fileId") String fileId);

    @ApiOperation(value = "Download iOS File")
    @GetMapping("/files/download-ios-app")
    ResponseEntity<InputStreamResource> downloadIosAppFile();

    @GetMapping("/files/{fileId}/view")
    @ApiOperation("View file")
    @PreAuthorize("hasPermission(null, 'file:view')")
    ResponseEntity<?> viewFileImage(@PathVariable("fileId") String fileId);

    @ApiOperation(value = "Search files")
    @GetMapping("/files")
    PagingResponse<File> search(@ValidatePaging(
            allowedSorts = {"lastModifiedAt", "createdAt"}) FileSearchRequest fileSearchRequest);

    @ApiOperation(value = "Delete file by id")
    @PostMapping("/files/{fileId}/delete")
    @PreAuthorize("hasPermission(null, 'file:update')")
    Response<File> deleteById(@PathVariable("fileId") String fileId);

    @ApiOperation(value = "Get file by id")
    @GetMapping("/files/{fileId}")
    @PreAuthorize("hasPermission(null, 'file:view')")
    Response<File> findById(@PathVariable("fileId") String fileId);

    @ApiOperation("Find file by ids")
    @PostMapping("/files/find-by-ids")
    @PreAuthorize("hasPermission(null, 'file:view')")
    Response<List<File>> findByIds(@RequestBody FindByIdsRequest request);

    @ApiOperation("Check before upload")
    @PostMapping("/files/check-server-available")
    @PreAuthorize("hasPermission(null, 'file:create')")
    Response<List<File>> checkServerAvailable();

    //    Public file
    @ApiOperation(value = "Upload multi file public")
    @PostMapping("/public/files/uploads")
    Response<List<File>> publicFileUploads(@RequestParam("files") List<MultipartFile> files);

    @ApiOperation(value = "Upload single file public")
    @PostMapping("/public/files/upload")
    Response<File> publicFileUpload(@RequestPart(name = "file") MultipartFile file);
}
