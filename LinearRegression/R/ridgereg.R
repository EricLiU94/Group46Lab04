library(ggplot2)
library(MASS)
#' Ridge Regression Package
#' @export ridgereg
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
                          t_values = "numeric"),
                        
                        methods =list (
                          initialize<- function(formula, data, lambda) {
                            X <- model.matrix(formula, data)
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
                            
                            input <<- paste("linreg(formula = ", deparse(formula), ", data = ", deparse(substitute(data)), ")", sep = "")
                            
                          }, 
                          print <- function () {
                            
                          },
                          predict <- function(){
                            return(fitted_y)
                          }
                          
                        )
)
                          