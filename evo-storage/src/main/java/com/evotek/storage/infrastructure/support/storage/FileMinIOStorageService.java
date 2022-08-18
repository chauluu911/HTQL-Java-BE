package com.evotek.storage.infrastructure.support.storage;

import com.evotek.common.exception.ResponseException;
import com.evotek.storage.domain.File;
import com.evotek.storage.infrastructure.support.exception.BadRequestError;
import com.evotek.storage.infrastructure.support.util.MD5Util;
import com.evotek.storage.infrastructure.support.util.StringUtil;
import io.minio.*;
import io.minio.errors.*;
import io.minio.messages.Bucket;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Primary;
import org.springframework.core.io.InputStreamResource;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.stream.Stream;

import static com.evotek.common.util.StringPool.SLASH;

@Slf4j
@Primary
@Service(value = "fileMinIo")
public class FileMinIOStorageService implements StorageService {

    private final MinioClient minioClient;
    @Value("${minio.bucket}")
    private String bucket;

    public FileMinIOStorageService(MinioClient minioClient) {
        this.minioClient = minioClient;
    }

    @Override
    public String store(MultipartFile file) throws IOException, ServerException, InsufficientDataException, ErrorResponseException, NoSuchAlgorithmException, InvalidKeyException, InvalidResponseException, XmlParserException, InternalException {
//        String fileName = File.genNewFileName(file.getOriginalFilename(), getHashedFileMd5(file));
        String fullName = StringUtil.getSafeFileName(Objects.requireNonNull(file.getOriginalFilename()));
        return this.store(file, fullName);
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

    private String dateFolderPath() {
        String format = "yyyy/MM/dd";
        SimpleDateFormat sdf = new SimpleDateFormat(format);
        return SLASH + sdf.format(new Date()).replace("\\", SLASH) + SLASH;
    }

    @Override
    public String store(MultipartFile file, String ownerFolderPath, String fileName) throws IOException, ServerException, InsufficientDataException, ErrorResponseException, NoSuchAlgorithmException, InvalidKeyException, InvalidResponseException, XmlParserException, InternalException {
        checkInitBucket(bucket);

        BufferedInputStream bufferedInputStream = new BufferedInputStream(file.getInputStream());
        PutObjectArgs putObjectArgs =
                PutObjectArgs.builder().bucket(bucket).object(ownerFolderPath + fileName).stream(
                                bufferedInputStream, file.getSize(), ObjectWriteArgs.MIN_MULTIPART_SIZE)
                        .build();
        ObjectWriteResponse objectWriteResponse = minioClient.putObject(putObjectArgs);
        return objectWriteResponse.object();
    }

    @Override
    public String store(MultipartFile file, String fileNameHashed) throws IOException, ServerException, InsufficientDataException, ErrorResponseException, NoSuchAlgorithmException, InvalidKeyException, InvalidResponseException, XmlParserException, InternalException {
        checkInitBucket(bucket);
        String folder = dateFolderPath() + fileNameHashed.substring(0, 2) + SLASH;

        BufferedInputStream bufferedInputStream = new BufferedInputStream(file.getInputStream());
        PutObjectArgs putObjectArgs =
                PutObjectArgs.builder().bucket(bucket).object(folder + fileNameHashed).stream(
                                bufferedInputStream, file.getSize(), ObjectWriteArgs.MIN_MULTIPART_SIZE)
                        .build();
        ObjectWriteResponse objectWriteResponse = minioClient.putObject(putObjectArgs);
        return objectWriteResponse.object();
    }

    @Override
    public InputStreamResource getFileInputStream(String filename) {
        try {
            InputStream inputStream = getFile(this.bucket, filename);
            return new InputStreamResource(inputStream);
        } catch (Exception e) {
            log.error("Error: ", e);
            throw new StorageFileNotFoundException("Could not read file: " + filename, e);
        }
    }

    public Mono<InputStreamResource> download(String fileName) {
        return Mono.fromCallable(() -> {
            try {
                return getFileInputStream(fileName);
            } catch (Exception e) {
                log.error("Error: ", e);
                throw new StorageFileNotFoundException("Could not read file: " + fileName, e);
            }
        }).subscribeOn(Schedulers.boundedElastic());
    }

    @Override
    public String getFolderPath(String fileName) {
        return dateFolderPath() +
                fileName.substring(0, 2) + SLASH;
    }

    public InputStream getFile(String buket, String filename) {
        try {
            GetObjectArgs getObjectArgs = GetObjectArgs.builder().bucket(buket).object(filename).build();
            InputStream obj = minioClient.getObject(getObjectArgs);
            return obj;
        } catch (Exception e) {
            throw new StorageFileNotFoundException("Could not read file: " + filename, e);
        }
    }

    @SneakyThrows
    private void checkInitBucket(String bucketName) {
        boolean isExist = true;
        try {
            isExist = minioClient
                    .bucketExists(BucketExistsArgs.builder()
                            .bucket(bucketName).build());
            if (isExist) {
                log.info("Bucket {} already exists.", bucketName);
            }
        } catch (Exception e) {
            log.error(e.getMessage());
            minioClient.makeBucket(MakeBucketArgs.builder().bucket(bucketName).build());
        }
    }

    private String getSuffix(String fileName) {
        String suffix = fileName.substring(fileName.lastIndexOf(".") + 1);
        return "." + suffix;
    }

    public Flux<Bucket> getAllBuckets() {
        try {
            return Flux.fromIterable(minioClient.listBuckets()).subscribeOn(Schedulers.boundedElastic());

        } catch (Exception e) {
            throw new RuntimeException(e.getMessage());
        }
    }
}
