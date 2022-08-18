package com.evotek.asset.application.mapper;

import com.evotek.asset.application.dto.request.AssetCreateRequest;
import com.evotek.asset.application.dto.request.AssetTypeCreateRequest;
import com.evotek.asset.application.dto.request.AssetTypeUpdateRequest;
import com.evotek.asset.application.dto.request.AssetUpdateRequest;
import com.evotek.asset.domain.command.AssetCreateCmd;
import com.evotek.asset.domain.command.AssetTypeCreateCmd;
import com.evotek.asset.domain.command.AssetTypeUpdateCmd;
import com.evotek.asset.domain.command.AssetUpdateCmd;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface AutoMapper {

   AssetTypeCreateCmd from(AssetTypeCreateRequest request);

   AssetTypeUpdateCmd from(AssetTypeUpdateRequest request);

   AssetCreateCmd from(AssetCreateRequest request);

   AssetUpdateCmd from(AssetUpdateRequest request);

}
