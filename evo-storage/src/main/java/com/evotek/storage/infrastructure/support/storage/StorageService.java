package com.evotek.storage.infrastructure.support.storage;

import io.minio.errors.*;
import org.springframework.core.io.InputStreamResource;
import org.springframework.web.multipart.MultipartFile;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;

public interface StorageService {

    String store(MultipartFile file) throws IOException, ServerException, InsufficientDataException, ErrorResponseException, NoSuchAlgorithmException, InvalidKeyException, InvalidResponseException, XmlParserException, InternalException;

    String store(MultipartFile filePart, String ownerFolderPath, String fileName) throws IOException, ServerException, InsufficientDataException, ErrorResponseException, NoSuchAlgorithmException, InvalidKeyException, InvalidResponseException, XmlParserException, InternalException;

    String store(MultipartFile filePart, String fileName) throws IOException, ServerException, InsufficientDataException, ErrorResponseException, NoSuchAlgorithmException, InvalidKeyException, InvalidResponseException, XmlParserException, InternalException;

//    Resource loadAsResource(String filename);

//    Stream<Path> loadAll();

    InputStreamResource getFileInputStream(String filename);

    Mono<InputStreamResource> download(String fileName);

    String getFolderPath(String fileName);
}
