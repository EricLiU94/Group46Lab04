library(ggplot2)
#' Ridge Regression Package
#' @description Ridge regression shrinks the regression coefficients by imposing a penalty on their size. The New element in this function is Lambda, the  effective degrees of freedom
ridgeQR <- setRefClass("ridgeQR",
                       fields = list ( 
                       ), 
                       methods = list(
                         initialize = function(formula, data, lambda) {
                           # to implement the QR method 
                           
                         }
                       )
) 