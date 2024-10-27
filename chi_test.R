#' Chi-Square Test Analysis
#'
#' @param data A data frame containing the data.
#' @param var1 The first categorical variable.
#' @param var2 The second categorical variable.
#' @return A frequency table, chi-square test result, and a conclusion based on the p-value.
#' @export
frequency <- function(data, var1, var2) {
  freq_table <- table(data[[var1]], data[[var2]])
  chisq_test <- chisq.test(freq_table)
  pvalue3 <- chisq_test$p.value
  
  if (pvalue3 < 0.05) {
    conclusion <- paste("There is a statistically significant association between", var1, "and", var2)
  } else {
    conclusion <- paste("There is no statistically significant association between", var1, "and", var2)
  }
  
  print(chisq_test)
  cat("Conclusion:", conclusion, "\n")
}