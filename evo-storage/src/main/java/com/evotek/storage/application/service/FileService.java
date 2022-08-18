package com.evotek.storage.application.service;

import com.evotek.common.dto.PageDTO;
import com.evotek.common.webapp.support.DomainService;
import com.evotek.storage.application.dto.request.FileSearchRequest;
import com.evotek.storage.application.dto.response.FileViewResponse;
import com.evotek.storage.domain.File;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.web.multipart.MultipartFile;
import reactor.core.publisher.Mono;

import java.util.List;

public interface FileService {

    /**
     * Check file existed
     *
     * @return FileUpload
     */
    File ensureExisted(String id);

    List<File> findByIds(List<String> ids);

    /**
     * Create the list file.
     *
     * @return the list
     */
    List<File> createFile(List<MultipartFile> files, String objectId, String objectType);

    /**
     * Create one file.
     *
     * @return File
     */
    File createFile(MultipartFile file, String objectId, String objectType);

    /**
     * Xóa mềm file
     *
     * @return File
     */
    File deleteFileById(String fileId);

    PageDTO<File> search(FileSearchRequest request);

    FileViewResponse getFileView(String fileId);
}
