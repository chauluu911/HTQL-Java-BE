package com.evotek.apigateway.fallback;

import com.evotek.apigateway.dto.Response;
import com.evotek.apigateway.error.InternalServerError;
import com.evotek.apigateway.exception.ResponseException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
public class FallbackController {

    @RequestMapping("fallback")
    public Response<Void> fallback() {
        throw new ResponseException(InternalServerError.SERVICE_UNAVAILABLE_ERROR);
    }
}
