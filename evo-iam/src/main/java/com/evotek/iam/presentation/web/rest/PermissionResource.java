package com.evotek.iam.presentation.web.rest;

import com.evotek.common.dto.response.Response;
import com.evotek.iam.domain.Permission;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.List;

@Api(tags = "Permission Resource")
@RequestMapping("/api")
@Validated
public interface PermissionResource {

    @ApiOperation(value = "Find all permissions")
    @GetMapping("/permissions")
    @PreAuthorize("hasPermission(null, 'role:view')")
    Response<List<Permission>> findAll();

}
