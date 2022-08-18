-- ----------------------------
-- Table structure for SCHEDULED_TASKS
-- ----------------------------
DROP TABLE "TICKET"."SCHEDULED_TASK";
CREATE TABLE "TICKET"."SCHEDULED_TASK"
(
    "TASK_NAME"            VARCHAR2(100 BYTE),
    "TASK_INSTANCE"        VARCHAR2(100 BYTE),
    "TASK_DATA"            BLOB,
    "EXECUTION_TIME"       TIMESTAMP(6),
    "PICKED"               NUMBER(1,0),
    "PICKED_BY"            VARCHAR2(50 BYTE),
    "LAST_SUCCESS"         TIMESTAMP(6),
    "LAST_FAILURE"         TIMESTAMP(6),
    "CONSECUTIVE_FAILURES" NUMBER(19,0),
    "LAST_HEARTBEAT"       TIMESTAMP(6),
    "VERSION"              NUMBER(19,0),
    PRIMARY KEY ("TASK_NAME", "TASK_INSTANCE")
);