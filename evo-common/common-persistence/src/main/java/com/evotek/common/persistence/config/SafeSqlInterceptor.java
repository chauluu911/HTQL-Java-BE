package com.evotek.common.persistence.config;

import org.hibernate.EmptyInterceptor;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SafeSqlInterceptor extends EmptyInterceptor {

    private static final Pattern patternLike = Pattern.compile("[^\\s]+\\s+like\\s*\\?", Pattern.CASE_INSENSITIVE);
    private static final Pattern patternIn = Pattern.compile("[^\\s]+\\s+in\\s*\\(\\s*\\?[^\\(]*\\)", Pattern.CASE_INSENSITIVE);
    private static final int IN_CAUSE_LIMIT = 1000;

    @Override
    public String onPrepareStatement(String sql) {
        //  sql = this.rewriteSqlToAvoidOra01795(sql);
        sql = this.rewriteSqlLike(sql);
        return super.onPrepareStatement(sql);
    }

    private String rewriteSqlToAvoidOra01795(String sql) {
        Matcher matcher = patternIn.matcher(sql);
        while (matcher.find()) {
            String inExpression = matcher.group();
            long countOfParameters = inExpression.chars().filter(ch -> ch == '?').count();

            if (countOfParameters <= IN_CAUSE_LIMIT) {
                continue;
            }

            String fieldName = inExpression.substring(0, inExpression.indexOf(' '));
            StringBuilder transformedInExpression = new StringBuilder(" ( ").append(fieldName).append(" in (");

            for (int i = 0; i < countOfParameters; i++) {
                if (i != 0 && i % IN_CAUSE_LIMIT == 0) {
                    transformedInExpression
                            .deleteCharAt(transformedInExpression.length() - 1)
                            .append(") or ").append(fieldName).append(" in (");
                }
                transformedInExpression.append("?,");
            }
            transformedInExpression.deleteCharAt(transformedInExpression.length() - 1).append("))))");
            sql = sql.replaceFirst(Pattern.quote(inExpression), transformedInExpression.toString());
        }
        return sql;
    }

    private String rewriteSqlLike(String sql) {
        Matcher matcher = patternLike.matcher(sql);
        while (matcher.find()) {
            String likeExpression = matcher.group();

            StringBuilder transformedInExpression = new StringBuilder();

            String fieldName = likeExpression.substring(0, likeExpression.indexOf(' '));

            while (fieldName.startsWith("(")) {
                fieldName = fieldName.substring(1);
                transformedInExpression.append(" ( ");
            }
            transformedInExpression.append(" f_unaccent(lower(").append(fieldName).append(")) like ").append("f_unaccent(lower(?))");

            sql = sql.replaceFirst(Pattern.quote(likeExpression), transformedInExpression.toString());
        }
        return sql;
    }
}
