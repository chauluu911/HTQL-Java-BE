package com.evotek.iam.infrastructure.support.util;

import com.evotek.common.util.DateUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import java.io.InputStream;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Slf4j
public class ExcelUtils {
    public static final int FONT_SIZE = 12;

    public static CellStyle createCellStyle(Workbook workbook, short fontIndex, String type) {
        CellStyle style = workbook.createCellStyle();

        Font font = workbook.createFont();

        font.setColor(fontIndex);

        // set font bold neu la ten cot
        if (Objects.nonNull(type) && type.equals("nameCell")) {
            font.setBold(true);
        } else {
            font.setItalic(true);
        }
        font.setFontHeight((short) (FONT_SIZE * 20));

        style.setAlignment(HorizontalAlignment.CENTER);
        style.setVerticalAlignment(VerticalAlignment.CENTER);
        font.setFontName("Times New Roman");
        style.setFont(font);
        style.setWrapText(true);

        return style;
    }

    public static String readCellContent(Cell cell) {
        String content;

        switch (cell.getCellType()) {

            case STRING:
                content = cell.getStringCellValue();

                break;
            case NUMERIC:
                if (DateUtil.isCellDateFormatted(cell)) {

                    content = DateUtils.formatDate(cell.getDateCellValue());
                } else {
                    content = BigDecimal.valueOf(cell.getNumericCellValue()).toString();
                }

                break;
            case BOOLEAN:
                content = cell.getBooleanCellValue() + "";

                break;
            case FORMULA:
                content = cell.getCellFormula() + "";

                break;
            default:
                content = "";
        }

        return content;
    }

    public static void createCell(Row row, int index, CellStyle style, String message) {
        Cell cell = row.createCell(index, CellType.STRING);
        cell.setCellValue(message);

        cell.setCellStyle(style);

        row.setRowStyle(style);
    }

    public static CellStyle createErrorCellStyle(Workbook workbook) {
        return createCellStyle(workbook, IndexedColors.RED.getIndex(), null);
    }

    public static CellStyle createSuccessCellStyle(Workbook workbook) {
        return createCellStyle(workbook, IndexedColors.BLUE.getIndex(), null);
    }

    public static CellStyle createNameCellStyle(Workbook workbook) {
        return createCellStyle(workbook, IndexedColors.BLACK.getIndex(), "nameCell");
    }

    public static CellStyle createValueCellStyle(Workbook workbook) {
        return createCellStyle(workbook, IndexedColors.BLACK1.getIndex(), null);
    }

    public static void removeEmptyRows(Sheet sheet) {
        Boolean isRowEmpty = Boolean.FALSE;
        for (int i = 0; i <= sheet.getLastRowNum(); i++) {
            if (sheet.getRow(i) == null) {
                isRowEmpty = true;
                sheet.shiftRows(i + 1, sheet.getLastRowNum() + 1, -1);
                i--;
                continue;
            }
            for (int j = 0; j < sheet.getRow(i).getLastCellNum(); j++) {
                if (sheet.getRow(i).getCell(j) == null ||
                        sheet.getRow(i).getCell(j).toString().trim().equals("")) {
                    isRowEmpty = true;
                } else {
                    isRowEmpty = false;
                    break;
                }
            }
            if (isRowEmpty) {
                sheet.shiftRows(i + 1, sheet.getLastRowNum() + 1, -1);
                i--;
            }
        }
    }

    public boolean isValidHeader(Sheet sheet, int headerIndex, String... headerTexts) {
        Row headerRow = sheet.getRow(headerIndex);

        if (headerRow == null) {
            return false;
        }

        int headerSize = headerTexts.length;

        if (headerRow.getPhysicalNumberOfCells() < headerSize || headerRow.getLastCellNum() < headerSize - 1) {
            return false;
        }

        for (int i = 0; i < headerSize; i++) {
            Cell cell = headerRow.getCell(i);

            if (cell == null) {
                return false;
            }

            String value = readCellContent(cell);

            if ((value == null && headerTexts[i] != null)
                    || (value != null && !value.equalsIgnoreCase(headerTexts[i]))) {
                return false;
            }
        }

        return true;
    }

    public String[] getHeaderText(InputStream is, int sheetIndex, int headerRowIndex) {
        List<String> headerTexts = new ArrayList<>();

        try (Workbook workbook = new XSSFWorkbook(is)) {
            Sheet sheet = workbook.getSheetAt(sheetIndex);

            Row headerRow = null;

            if (Objects.isNull(sheet)
                    || Objects.isNull(headerRow = sheet.getRow(headerRowIndex))) {
                return null;
            }

            for (int i = 0; i < headerRow.getLastCellNum(); i++) {
                Cell cell = headerRow.getCell(i);

                if (cell == null) {
                    continue;
                }

                String value = readCellContent(cell);

                headerTexts.add(value);
            }
        } catch (Exception e) {
            log.error("Cannot read the template header");
        }

        return headerTexts.toArray(new String[0]);
    }

    public boolean checkIfRowIsEmpty(Row row) {
        if (row == null || row.getLastCellNum() <= 0) {
            return true;
        }

        for (int cellNum = row.getFirstCellNum(); cellNum < row.getLastCellNum(); cellNum++) {

            Cell cell = row.getCell(cellNum);

            if (cell != null && !cell.getCellType().equals(CellType.BLANK)
                    && Objects.nonNull(cell.toString())) {
                return false;
            }
        }

        return true;
    }
}


