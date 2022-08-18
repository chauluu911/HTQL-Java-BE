package com.evotek.iam.domain;

import com.evotek.iam.domain.command.ClientCreateCmd;
import com.evotek.iam.infrastructure.support.enums.ClientStatus;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ClientTest {
    @Test
    @DisplayName("Test create new client by a given name, should pass if client name equals with given name")
    void testCreateClientByName() {
        final String name = "testName";
        ClientCreateCmd cmd = ClientCreateCmd.builder().name(name).build();
        Client client = new Client(cmd);
        assertEquals(client.getName(), name);
    }
}
