package com.evotek.common.dto.response.system;

import com.evotek.common.enums.ConfigurationStatus;
import com.evotek.common.enums.ConfigurationType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ConfigurationDTO implements Serializable {
    private String id;
    private String name;
    private String code;
    private String value;
    private Boolean deleted;
    private ConfigurationType type;
    private ConfigurationStatus status;
    private String createdUserId;
}
