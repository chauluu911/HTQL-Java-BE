package com.evotek.common.validator;

import javax.validation.ConstraintValidatorContext;

public final class ValidatorUtils {
    public static void createErrorField(
            ConstraintValidatorContext context, String field, String message, boolean disableDefaultConstraint) {
        if (disableDefaultConstraint) {
            context.disableDefaultConstraintViolation();
        }
        context.buildConstraintViolationWithTemplate(message)
                .addPropertyNode(field)
                .addConstraintViolation();
    }
}
