library(ggplot2)
library(MASS)
#' Ridge Regression Package
#' @export ridgereg
#' @field input, the input variables, formula and data
#' @field var_name the variable names
#' @field regres_coef contains the estimated beta value. If the scrutinised system contains one dependent variable, the number of element in regres_coef is two, beta_zero and beta one. On the other hand, if two dependent variables are included, the number of beta elements would be three. 
#' @field fitted_y, is the estimated value, using the regression coefficients
#' @field res_value, is the residuals, the difference between the sampling points and the estimated value
#' @field degrees_of_freedom, the degree of freedom describe the degress of freedom of the scrutinised system 
#' @field res_value, the residual variance is the variance of the error, between the estimated value and the sampling value 
#' @field standard_resvec, the standard residuals gives a measure of the strength of the difference between observed and expected values
#' @field res_var, the variance of the regression coefficients
#' @field t_values, the t-values for each coefficient
#' @field p_value, the p-values for each coefficient
#' @field sq_standard_resvec gives the square root of the standard residual vector value
#' @param formula a relation between variables
#' @param data the data set containing variable values
#' @param lambda the penalty coefficient
#' @return a rigid regression object containing relevant paramters
#' @description Ridge regression shrinks the regression coefficients by imposing a penalty on their size. The New element in this function is Lambda, the  effective degrees of freedom
ridgereg <- setRefClass("ridgereg",
                        fields = list ( 
                          var_name = "character",
                          input = "character",
                          degrees_of_freedom = "numeric",
                          regres_coef = "matrix",
                          fitted_y = "matrix",
                          standard_resvec = "matrix",
                          sq_standard_resvec = "matrix",
                          regression_var = "matrix",
                          res_var = "numeric",
                          p_value = "numeric",
                          t_values = "numeric" 
                          ),
                        
                        methods =list (
                          initialize = function(formula, data, lambda) {
                            X <- model.matrix(formula, data)
                            degrees_of_freedom <<- dim(X)[1]-dim(X)[2]
                            var_name <<- colnames(X)
                            degrees_of_freedom <<- dim(X)[1]-dim(X)[2]
                            # to calculate the normalised X 
                            templ<- dim(X)[2] 
                            for (i in 2:templ){
                              tempx1<- X[ ,i]
                              tempx2<- sapply(tempx1, function(k) (k-mean(tempx1))/var(tempx1))
                              X[ ,i]<- tempx2
                            }
                            
                            # extarcting the independent variable
                            t1 <- all.vars(formula)
                            
                            # to change the class of the data from a data frame to a list
                            f1 <- which(names(data) == t1[1])
                            y1 <- data[f1]
                            Y <- as.matrix(y1)
                            
                            # to create an identity matrix I
                            felement <- dim(X)
                            len<- felement[2]
                            enhet<-matrix(1:(len*len), ncol =len)*0
                            
                            for (i in 1:len){
                              enhet[i,i]=1
                            }
                              
                           regres_coef <<- (solve(t(X) %*% X) + lambda * enhet) %*% ( t(X) %*% Y)  # the beta coefficient
                           fitted_y <<- X %*% regres_coef
                            
                            res_value <- Y -fitted_y # the residue vector 
                            
                            standard_resvec<<- (res_value -mean(res_value))/sd(res_value)
                            
                            sq_standard_resvec<<- sqrt(abs(standard_resvec)) 
                            
                            e<- res_value   # the estimated residue 
                            
                            res_var <<- as.numeric(t(e) %*% e)/(degrees_of_freedom) 
                            
                            sigma2 <- res_var
                            regression_var <<- solve(t(X) %*% X)*sigma2
                            
                            tempt1 <- sapply(diag(regression_var), sqrt) 
                            temptt2 <- c(1:length(tempt1))
                            temptt3<- as.numeric(regres_coef)
                            temptt2<- temptt3 /tempt1
                            
                            t_values <<-  temptt2 
                            p_value <<- 2*pt(-abs(t_values) , degrees_of_freedom)
                            
                            input <<- paste("ridgereg(formula = ", deparse(formula), ", data = ", deparse(substitute(data)), ", lambda = ", deparse(substitute(lambda)), ")", sep = "") 
                            
                          }, 
                          
                          predict = function ( ){
                            ' Retuns the fitted value y'
                            colnames(fitted_y) <<- NULL
                            return(t(as.matrix(fitted_y))) 
                          },
                          
                          print = function ( ) {
                            'This is a print out function, which outlines the regression coefficients of the given system'
                            cat("Call:\n")
                            cat(noquote(input), "\n\n")
                            cat("Coefficients:\n")
                            base::print(t(regres_coef), row.names = FALSE)
                          }, 
                          coef = function(){ 
                            'Returns the regression coefficient, as a function of formula, data, also the penalty coefficient '
                            return(regres_coef)
                          }
                   
                        ) 
)
                          