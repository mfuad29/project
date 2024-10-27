#' Linear Regression Analysis
#'
#' @param data A data frame containing the data.
#' @param response_var The response variable.
#' @param predictor_var The predictor variable.
#' @return A summary of the linear regression model and a conclusion based on the p-value.
#' @export
regression <- function(data, response_var, predictor_var) {
  formula <- as.formula(paste(response_var, "~", predictor_var))
  model <- lm(formula, data = data)
  model_summary <- summary(model)
  
  coefficient <- model_summary$coefficients
  pvalue <- coefficient[2, 4]
  
  if (pvalue < 0.05) {
    conclusion <- paste("There is a significant relationship between", predictor_var, "and", response_var)
  } else {
    conclusion <- paste("There is no significant relationship between", predictor_var, "and", response_var)
  }
  
  print(model_summary)
  cat("Conclusion:", conclusion, "\n")
}
