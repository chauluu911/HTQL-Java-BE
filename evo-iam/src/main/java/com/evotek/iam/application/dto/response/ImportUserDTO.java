package com.evotek.iam.application.dto.response;

import com.evotek.iam.domain.command.ImportUserCmd;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Data
@NoArgsConstructor
@Getter
@Setter
public class ImportUserDTO {
    private int rowIndex;
    private ImportUserCmd value;
    private Boolean check;
    private StringBuilder errors;
}
