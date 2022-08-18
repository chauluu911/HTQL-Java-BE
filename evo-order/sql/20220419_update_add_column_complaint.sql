-- --------------------------------------
-- UPDATE COLUMN IN COMPLAINT
-- --------------------------------------
ALTER TABLE "COMPLAINT_STATUS_HISTORY"
ADD NOTE varchar2(1000);

ALTER TABLE "COMPLAINT"
ADD EMAIL varchar2(50);