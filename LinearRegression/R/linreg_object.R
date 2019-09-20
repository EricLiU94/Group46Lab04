#' A Reference Class to represent a linear regression
#'
#' @field a A numeric something
linreg <- setRefClass("linreg", fields = list(a = "numeric",
                                              b = "list",
                                              c = "matrix"))