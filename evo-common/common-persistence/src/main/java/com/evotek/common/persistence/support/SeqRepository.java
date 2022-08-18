package com.evotek.common.persistence.support;

import org.springframework.transaction.annotation.Transactional;

import java.math.BigInteger;
import java.time.LocalDate;

public interface SeqRepository {

    BigInteger nextValue(String seqName);

    @Transactional
    default String nextValue(String prefix, String seqName) {
        return prefix + nextValue(seqName);
    }

    @Transactional
    default String generateComplaintCode() {
        int year = LocalDate.now().getYear();
        return nextValue(Const.COMPLAINT_CODE.getPrefix() + year, Const.COMPLAINT_CODE.getSeqName());
    }

    @Transactional
    default String generateOrderCode() {
        String dateNow = LocalDate.now().toString().replaceAll("-", "") + "_";
        return nextValue(dateNow, Const.ORDER_CODE.getSeqName());
    }

    @Transactional
    default String generateProductCode() {
        String dateNow = LocalDate.now().toString().replaceAll("-", "") + "_";
        return nextValue(dateNow, Const.PRODUCT_CODE.getSeqName());
    }

    @Transactional
    default String generateMenuCode() {
        String dateNow = LocalDate.now().toString().replaceAll("-", "") + "_";
        return nextValue(dateNow, Const.MENU_CODE.getSeqName());
    }

    @Transactional
    default String generateOrganizationCode() {
        return nextValue(Const.ORGANIZATION_CODE.getPrefix(), Const.ORGANIZATION_CODE.getSeqName());
    }

    @Transactional
    default String generateAssetCode(String productType) {
        return nextValue(productType + "-" + Const.ASSET_CODE.getPrefix() + "-", Const.ASSET_CODE.getSeqName());
    }


}
