#The object
linreg <- setRefClass("linreg",
                      fieldvariables <- list(
                        regres_coef = "matrix",
                        fitted_y = "matrix",
                        res_value = "matrix",
                        degrees_of_freedom = "numeric",
                        res_var = "numeric",
                        regression_var = "matrix",
                        t_values = "numeric",
                        p_value = "matrix"),
                      methods = list(
                        initialize = function(formula, data) {
                          # the dependent variable
                          X <- model.matrix(formula, data)
                          
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
                          p<-2 # two parameters, beta zero and beta one
                          degrees_of_freedom <<- f[1]-p
                          
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
                          p_value <<- pt(regres_coef , df)
                        }
                      )
)