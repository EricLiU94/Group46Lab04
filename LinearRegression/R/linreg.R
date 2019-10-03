#' Linear Regression package
#' 
#' @export linreg
#' @field regres_coef contains the estimated beta value. If the scrutinised system contains one dependent variable, the number of element in regres_coef is two, beta_zero and beta one. On the other hand, if two dependent variables are included, the number of beta elements would be three. 
#' @field fitted_y, is the estimated value, using the regression coefficients
#' @field res_value, is the residuals, the difference between the sampling points and the estimated value
#' @field the degree of freedom is
#' @field
#' @param formula a relation between variables
#' @param datathe data set containing variable values
#' @return a linear regression object containing relevant paramters
linreg <- setRefClass("linreg",
                      fields = list(
                        input = "character",
                        var_name = "character",
                        regres_coef = "matrix",
                        fitted_y = "matrix",
                        res_value = "matrix",
                        degrees_of_freedom = "numeric",
                        res_var = "numeric",
                        regression_var = "matrix",
                        t_values = "numeric",
                        p_value = "numeric"),
                      methods = list(
                        initialize = function(formula, data) {
                          input <<- paste("linreg( formula =", deparse(formula), ", data =", deparse(substitute(data)), ")")
                          # the dependent variable
                          X <- model.matrix(formula, data)
                          var_name <<- colnames(X)
                          
                          # extarcting the independent variable
                          t1 <- all.vars(formula)
                          
                          # to change the class of the data from a data frame to a list
                          f1 <- which(names(data) == t1[1])
                          y1 <- data[f1]
                          Y <- as.matrix(y1)
                          
                          regres_coef <<- (solve(t(X) %*% X)) %*% ( t(X) %*% Y)
                          
                          fitted_y <<- X %*% regres_coef
                          
                          res_value <<- Y -fitted_y
                          
                          f <- dim(X)
                          p<-f[2]  # two parameters, beta zero and beta one
                          degrees_of_freedom <<- f[1]-f[2]
                          
                          e<- res_value   # the estimated residue 
                          
                          res_var <<- as.numeric(t(e) %*% e)/(degrees_of_freedom) 
                          
                          sigma2 <- res_var
                          regression_var <<- solve(t(X) %*% X)*sigma2
                          
                          tempt1 <- sapply(diag(regression_var), sqrt) 
                          temptt2 <- c(1:length(tempt1))
                          temptt3<- as.numeric(regres_coef)
                          temptt2<- temptt3 /tempt1
                          
                          t_values <<-  temptt2 
                          df<- degrees_of_freedom
                          p_value <<- 2*pt(-abs(t_values) , df) 
                        }, 
                        
                        resid = function() {
                          print(paste("The vector of residuals is")) 
                          return(res_value)
                        }, 
                        
                        pred= function(){
                          return(fitted_y)
                        }, 
                        
                        coef= function(){ 
                          beta<-as.vector(regres_coef )
                          names(beta)<-rownames(regres_coef)
                          return(beta)
                        },
                        summary=function() { 
                          l1<-length(p_value)
                          l2<-1:l1
                          cat("The Included Regression Coefficients:\n")
                          cat("           ", "coefficients", "residuals", "  t-value", " p-value", "\n")
                          for (i in l2){
                            cat(var_name[i], format(regres_coef[i], digits = 5),format(res_value[i], digits = 5),
                                format(t_values[i], digits = 5), format(p_value[i], digits = 5),sep = "   ")
                            cat("\n")
                          }
                          cat("\n")
                          cat("The estimated residual variance is:", round(sqrt(res_var ), digits = 5))
                          cat("\n")
                          cat("The expected degree of freedom is:", degrees_of_freedom  )
                          cat("\n")
                        }
                      )
)
#' A function to print relevant regression output
#'
#' @param x a linreg object
#' @exportMethod print
setMethod("print", "linreg", function(x) {
  cat("Call:\n")
  cat(noquote(x$input), "\n\n")
  cat("Coefficients:\n")
  print(t(x$regres_coef), row.names = FALSE)
  }
  )
