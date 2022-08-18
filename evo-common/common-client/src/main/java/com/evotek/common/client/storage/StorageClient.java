package com.evotek.common.client.storage;

import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.storage.FileDTO;
import org.springframework.cloud.client.loadbalancer.LoadBalanced;
import org.springframework.cloud.loadbalancer.annotation.LoadBalancerClient;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.core.io.Resource;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

@LoadBalancerClient(name = "storage")
@FeignClient(name = "storage", fallbackFactory = StorageClientFallback.class)
public interface StorageClient {

    @GetMapping("/api/files/{fileId}")
    @LoadBalanced
    Response<FileDTO> findById(@PathVariable("fileId") String fileId);

    @PostMapping("/api/public/files/find-by-ids")
    @LoadBalanced
    Response<List<FileDTO>> findFileByIds(@RequestBody FindByIdsRequest request);

    @GetMapping("/api/files/{fileId}/view")
    @LoadBalanced
    ResponseEntity<Resource> viewFileImage(@PathVariable("fileId") String fileId);

    @GetMapping("/api/files/{fileId}/download")
    @LoadBalanced
    ResponseEntity<byte[]> downloadFileById(@PathVariable("fileId") String fileId);

    @PostMapping(value = "/api/files/upload", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @LoadBalanced
    Response<FileDTO> fileUpload(@RequestPart(name = "file") MultipartFile file,
                                 @RequestParam(value = "ownerId", required = false) String ownerId,
                                 @RequestParam(value = "ownerType", required = false) String ownerType);

}
