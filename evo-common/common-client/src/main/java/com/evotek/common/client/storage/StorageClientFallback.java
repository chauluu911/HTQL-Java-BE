package com.evotek.common.client.storage;

import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.storage.FileDTO;
import com.evotek.common.error.ServiceUnavailableError;
import com.evotek.common.exception.ForwardInnerAlertException;
import com.evotek.common.exception.ResponseException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cloud.openfeign.FallbackFactory;
import org.springframework.core.io.Resource;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

@Component
public class StorageClientFallback implements FallbackFactory<StorageClient> {

    @Override
    public StorageClient create(Throwable cause) {
        return new FallbackWithFactory(cause);
    }

    @Slf4j
    static class FallbackWithFactory implements StorageClient {

        private final Throwable cause;

        FallbackWithFactory(Throwable cause) {
            this.cause = cause;
        }

        @Override
        public Response<FileDTO> findById(String fileId) {
            log.error("Get file by id {} error", fileId, cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.STORAGE_SERVICE_UNAVAILABLE_ERROR));
        }

        @Override
        public Response<List<FileDTO>> findFileByIds(FindByIdsRequest request) {
            log.error("Get files by ids {} error", request, cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.STORAGE_SERVICE_UNAVAILABLE_ERROR));
        }

        @Override
        public ResponseEntity<Resource> viewFileImage(String fileId) {
            return ResponseEntity.badRequest().build();
        }

        @Override
        public ResponseEntity<byte[]> downloadFileById(String fileId) {
            return ResponseEntity.badRequest().build();
        }

        @Override
        public Response<FileDTO> fileUpload(MultipartFile file, String ownerId, String ownerType) {
            log.error("fileUpload {} error", cause);
            if (cause instanceof ForwardInnerAlertException) {
                return Response.fail((RuntimeException) cause);
            }
            return Response.fail(new ResponseException(ServiceUnavailableError.STORAGE_SERVICE_UNAVAILABLE_ERROR));
        }
    }
}
