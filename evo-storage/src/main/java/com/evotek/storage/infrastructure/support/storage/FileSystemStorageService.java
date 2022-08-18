package com.evotek.storage.infrastructure.support.storage;

import com.evotek.common.error.BadRequestError;
import com.evotek.common.exception.ResponseException;
import com.evotek.storage.application.config.StorageProperties;
import com.evotek.storage.infrastructure.support.util.MD5Util;
import com.evotek.storage.infrastructure.support.util.StringUtil;
import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.InputStreamResource;
import org.springframework.stereotype.Service;
import org.springframework.util.ResourceUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;
import reactor.core.publisher.Mono;

import java.io.*;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;
import java.util.UUID;

import static com.evotek.common.util.StringPool.DOT;
import static com.evotek.common.util.StringPool.SLASH;

@Service
public class FileSystemStorageService implements StorageService {
    private final Logger log = LoggerFactory.getLogger(FileSystemStorageService.class);

    private final Path rootLocation;

    private final StorageProperties storageProperties;

    public FileSystemStorageService(StorageProperties storageProperties) {
        this.storageProperties = storageProperties;
        this.rootLocation = Paths.get(storageProperties.getFolderUpload());
    }

    private String dateFolderPath() {
        String format = "yyyy/MM/dd";
        SimpleDateFormat sdf = new SimpleDateFormat(format);
        return SLASH + sdf.format(new Date()).replace("\\", SLASH) + SLASH;
    }

    @Override
    public String store(MultipartFile file) {
        String newFileName = "";
        try {
            if (file.isEmpty()) {
                throw new StorageException("Failed to store empty file " + file.getOriginalFilename());
            }
            String fullName = StringUtil.getSafeFileName(Objects.requireNonNull(file.getOriginalFilename()));
            String fileType = "";
            if (StringUtils.hasText(fullName)) {
                int last = fullName.lastIndexOf(DOT);
                if (last >= 0) {
                    fileType = fullName.substring(last);
                }
            }
            newFileName = MD5Util.MD5(file.getBytes()) + UUID.randomUUID().toString().substring(0, 10) + fileType;
            String folder = this.dateFolderPath();
            Path path = Paths.get(rootLocation.toString(), folder + newFileName);

            if (!Files.exists(Paths.get(rootLocation.toString(), folder))) {
                new File(Paths.get(rootLocation.toString(), folder).toString()).mkdirs();
            }
            Files.copy(file.getInputStream(), path);
            return path.toString();
        } catch (FileAlreadyExistsException e) {
            log.error("store file error", e);
            return newFileName;
        } catch (IOException e) {
            throw new StorageException("Failed to store file " + file.getOriginalFilename(), e);
        }
    }

    @Override
    public String store(MultipartFile file, String fileName) {
        return store(file, null, fileName);
    }

    @Override
    public String store(MultipartFile file, String ownerFolderPath, String fileName) {
        try {
            if (file.isEmpty()) {
                throw new StorageException("Failed to store empty file " + file.getOriginalFilename());
            }
            String folder;
            if (StringUtils.hasText(ownerFolderPath)) {
                folder = SLASH + ownerFolderPath + this.getFolderPath(fileName);
            } else {
                folder = this.dateFolderPath() + fileName.substring(0, 2) + SLASH;
            }
            String subPath = folder + fileName;

            if (!Files.exists(Paths.get(rootLocation.toString(), folder))) {
                new File(Paths.get(rootLocation.toString(), folder).toString()).mkdirs();
            }

            Path path = Paths.get(rootLocation.toString(), subPath);
            Files.copy(file.getInputStream(), path);
            return subPath;
        } catch (FileAlreadyExistsException e) {
            log.error("store file error", e);
            throw new ResponseException(String.format(BadRequestError.INVALID_INPUT.getMessage(),
                    file.getOriginalFilename()), BadRequestError.INVALID_INPUT);
        } catch (IOException e) {
            throw new StorageException("Failed to store file " + file.getOriginalFilename(), e);
        }
    }

//    @Override
//    public Stream<Path> loadAll() {
//        try {
//            Path rootPath = Paths.get(this.storageProperties.getFolderUpload());
//            return Files.walk(rootPath, 1)
//                    .filter(path -> !path.equals(rootPath))
//                    .map(rootPath::relativize);
//        } catch (IOException e) {
//            throw new StorageException("Failed to read stored files", e);
//        }
//    }

    @Override
    public InputStreamResource getFileInputStream(String filePath) {
        try {
            File file = ResourceUtils.getFile(Paths.get(rootLocation.toString(), filePath).toString());
            byte[] data = FileUtils.readFileToByteArray(file);
            InputStream inputStream = new BufferedInputStream(new ByteArrayInputStream(data));
            return new InputStreamResource(inputStream);
        } catch (Exception e) {
            log.error("Error: ", e);
            throw new StorageFileNotFoundException("Could not read file: " + filePath, e);
        }
    }

    @Override
    public Mono<InputStreamResource> download(String fileName) {
        return null;
    }

    @Override
    public String getFolderPath(String fileName) {
        return dateFolderPath() +
                fileName.substring(0, 2) + SLASH;
    }

//    @Override
//    public Path load(String filename) {
//        Path path = Paths.get(this.storageProperties.getFolderUpload());
//        return path.resolve(filename);
//    }

//    @Override
//    public Resource loadAsResource(String filePath) {
//        try {
//            Path file = Paths.get(rootLocation.toString(), filePath);
//            return new UrlResource(file.toUri());
//        } catch (MalformedURLException e) {
//            throw new StorageFileNotFoundException("Could not read file: " + filePath, e);
//        }
//    }
}
