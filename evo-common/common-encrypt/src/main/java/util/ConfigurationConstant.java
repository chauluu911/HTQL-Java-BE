package util;

public interface ConfigurationConstant {
    interface Dialog {
        String TITLE = "Tool Encrypt Configuration Information";
    }

    interface Jasypt {
        String ALGORITHM = "PBEWITHHMACSHA512ANDAES_256";
        Integer POOL_SIZE = 2;
    }
}

