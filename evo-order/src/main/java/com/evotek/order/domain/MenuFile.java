package com.evotek.order.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.dto.response.storage.FileDTO;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.IdUtils;
import com.evotek.order.infrastructure.support.exception.BadRequestError;
import com.evotek.order.infrastructure.support.util.Const;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Setter;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;


@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
@Setter
@Getter
@SuperBuilder
public class MenuFile extends AuditableDomain {
    private String id;
    private String menuId;
    private String fileId;
    private Boolean deleted;
    private String originalName;
    private String viewUrl;
    private String downloadUrl;

    public MenuFile(String menuId, FileDTO file) {
        if (!Const.WHITELIST_FILE_TYPES.contains(file.getType())) {
            throw new ResponseException(BadRequestError.FILE_MUST_BE_IMAGE);
        }
        this.id = IdUtils.nextId();
        this.menuId = menuId;
        this.fileId = file.getId();
        this.deleted = Boolean.FALSE;
    }

    public void delete() {
        this.deleted = Boolean.TRUE;
    }
}
