library(ggplot2)
library(MASS)
#' Ridge Regression Package
#' @export ridgereg
#' @description Ridge regression shrinks the regression coefficients by imposing a penalty on their size. The New element in this function is Lambda, the  effective degrees of freedom
ridgereg <- setRefClass("ridgereg",
                        fields = list ( 
                          regres_coef = "matrix",
                          fitted_y = "matrix"
                        #  regression_var = "matrix"
                          ),
                        methods =list (
                          initialize<- function(formula, data, lambda) {
                            X <- model.matrix(formula, data)
                            var_name <<- colnames(X)
                            
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
                            
                            
                          }
                        )
)
                          