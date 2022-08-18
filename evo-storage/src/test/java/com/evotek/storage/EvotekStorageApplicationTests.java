package com.evotek.storage;

import com.evotek.storage.infrastructure.support.util.StringUtil;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Objects;

@SpringBootTest
class EvotekStorageApplicationTests {

    @Test
    void contextLoads() {
        String fullName = StringUtil.getSafeFileName(Objects.requireNonNull("any.jpg"));
        System.out.println(fullName);
    }

}
