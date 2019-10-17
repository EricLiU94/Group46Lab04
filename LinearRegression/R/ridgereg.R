library(ggplot2)
#' Ridge Regression Package
ridgereg <- setRefClass("ridgereg",
                        fields = list ( 
                          xnorm = "matrix" 
                          ),
                        methods =list (
                          initialize<- function(formula, data, lambda) {
                            X <- model.matrix(formula, data)
                            var_name <<- colnames(X)
                            
                            # extarcting the independent variable
                            t1 <- all.vars(formula)
                            
                            # to change the class of the data from a data frame to a list
                            f1 <- which(names(data) == t1[1])
                            y1 <- data[f1]
                            Y <- as.matrix(y1)
                          }
                        ) ) 