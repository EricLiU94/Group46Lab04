#' Perform linear regression
#' 
#' @export linreg
#' @param formula An formula
#' @param data A data.frame
#' @return A linear regression in \code{RegressionValues}
linreg <- function(formula, data) {
  stopifnot(class(formula) == "formula", is.data.frame(data))
  RegressionValues <- linreg$new
  return(RegressionValues)
}