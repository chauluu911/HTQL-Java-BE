package com.evotek.storage.application.dto.response;

import com.evotek.storage.domain.File;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.springframework.core.io.InputStreamResource;

import javax.validation.constraints.NotNull;

@Getter
@AllArgsConstructor
public class FileViewResponse {
    @NotNull
    private File file;
    @NotNull
    private InputStreamResource inputStreamResource;
}
