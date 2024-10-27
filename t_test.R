#' T-Test Analysis
#'
#' @param data A data frame containing the data.
#' @param group_var The grouping variable.
#' @param numeric_var The numeric variable.
#' @return A summary of the t-test and a conclusion based on the p-value.
#' @export
ttest <- function(data, group_var, numeric_var) {
  meanvalues <- data %>%
    group_by(!!sym(group_var)) %>%
    summarise(meanvalues = mean(!!sym(numeric_var), na.rm = TRUE))
  
  ttest_result <- t.test(reformulate(group_var, numeric_var), data = data, var.equal = TRUE)
  pvalue2 <- ttest_result$p.value
  
  if (pvalue2 < 0.05) {
    conclusion <- paste("There is a statistically significant difference in", numeric_var, "between the groups in", group_var)
  } else {
    conclusion <- paste("There is no statistically significant difference in", numeric_var, "between the groups in", group_var)
  }
  
  print(ttest_result)
  cat("Conclusion:", conclusion, "\n")
}
